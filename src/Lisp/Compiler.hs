{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lisp.Compiler ( compile
                     , compileValues
                     , compile'
                     , compileValues'
                     ) where

import Prelude hiding (id, length)
import Data.Foldable (foldrM)
import Data.Maybe
import Data.Sequence

import Lisp.Data
import Lisp.Monad
import Lisp.VirtualMachine

compile :: Value -> LispM (Seq Instruction)
compile Nil = return [Push Nil]
compile (Symbol id) = return [Get id]
compile (Quote lit) = return [Push lit]
compile (List fn args) = compileFuncall fn args
compile val@(DottedList _ _ _) = raiseCompileDottedList val
compile lit = return [Push lit]

compileValues :: Seq Value -> LispM (Seq Instruction)
compileValues = concatM compile

compile' :: Value -> LispM (Seq Instruction)
compile' val = do
  insns <- compile val
  let go (Recur _)= raiseDisallowedForm "recur"
      go Return = raiseDisallowedForm "return"
      go insn = return insn
  mapM go insns

compileValues' :: Seq Value -> LispM (Seq Instruction)
compileValues' = concatM compile'

compileFuncall :: Value -> Seq Value -> LispM (Seq Instruction)
compileFuncall fn@(List _ _) args =
  fmap (|> Funcall (length args)) $ (><) <$> compile fn <*> compileValues args
compileFuncall (Symbol fn) args =
  let macroExpand (Macro (Left (_, native))) = Just $ native args
      macroExpand (Macro (Right func)) = Just $ funcall (Right func) args >>= compile
      macroExpand _ = Nothing
      makeFuncall = (|> Funcall (length args)) <$> ((Get fn <|) <$> compileValues' args)
  in  lookupSymbol' fn >>= \val -> fromMaybe makeFuncall $ val >>= macroExpand
compileFuncall val _ = raiseTypeMismatch "function or macro" val

concatM :: (Monad m, Foldable f, Monoid b) => (a -> m b) -> f a -> m b
concatM f = foldrM (\x ys -> flip mappend ys <$> f x) mempty
