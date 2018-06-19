{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lisp.Compiler ( compile
                     , compileValues
                     ) where

import Prelude hiding (id, length)
import Data.Foldable (foldrM)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Sequence (Seq, length)

import Lisp.Data
import Lisp.Core
import Lisp.VirtualMachine (funcall)

compile :: Value -> LispM Instruction
compile Nil = return $ Push Nil Halt
compile (Symbol id) = return $ Get id Halt
compile (Quote lit) = return $ Push lit Halt
compile (List fn args) = compileFuncall fn args
compile val@(DottedList _ _ _) = raiseCompileDottedList val
compile lit = return $ Push lit Halt

compileValues :: Seq Value -> LispM Instruction
compileValues = concatM compile

compileFuncall :: Value -> Seq Value -> LispM Instruction
compileFuncall fn@(List _ _) args =
  (<>) <$> compile fn
       <*> ((<>) <$> compileValues args
                 <*> pure (Funcall (length args) Halt))
compileFuncall (Symbol fn) args =
  let macroExpand (Macro (Left (_, native))) = Just $ native args
      macroExpand (Macro (Right func)) = Just $ funcall (Right func) args >>= compile
      macroExpand _ = Nothing
      makeFuncall = (<>) <$> (Get fn <$> compileValues args) <*> (pure $ Funcall (length args) Halt)
  in  lookupSymbol' fn >>= \val -> fromMaybe makeFuncall $ val >>= macroExpand
compileFuncall val _ = raiseTypeMismatch "function or macro" val

concatM :: (Monad m, Foldable f, Monoid b) => (a -> m b) -> f a -> m b
concatM f = foldrM (\x ys -> (<> ys) <$> f x) mempty
