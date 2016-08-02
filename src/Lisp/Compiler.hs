{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lisp.Compiler (compile, compileValues) where

import Prelude hiding (foldr, id, length, null, reverse)
import Control.Monad.Except
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
compile cons@(Cons _ _) = either (const $ throwError CompileDottedList) (return . viewl) (toSeq cons)
                      >>= compileFuncall
compile lit = return [Push lit]

compileValues :: Seq Value -> LispM (Seq Instruction)
compileValues = foldrM (\val acc -> (acc ><) <$> compile val) []

compileFuncall :: ViewL Value -> LispM (Seq Instruction)
compileFuncall EmptyL = return [Push Nil]
compileFuncall (cons@(Cons _ _) :< args) =
  fmap (|> Funcall (length args)) $ (><) <$> compile cons <*> compileValues args
compileFuncall (Symbol fn :< args) =
  let macroExpand (Macro (Left (_, native)) _) = Just $ native args
      macroExpand (Macro (Right compiled) scopeIDs) =
        let insns = Push (Lambda (Right compiled) scopeIDs)
                 <| (fmap Push (reverse args) |> Funcall (length args))
        in  Just $ eval insns >>= compile
      macroExpand _ = Nothing
      makeFuncall = (|> Funcall (length args)) <$> ((Get fn <|) <$> compileValues args)
  in  lookupSymbol' fn >>= \val -> fromMaybe makeFuncall $ val >>= macroExpand
compileFuncall _ = throwError $ TypeMismatch "function or macro"
