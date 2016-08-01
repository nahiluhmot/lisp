{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lisp.Compiler (compile, compileValues) where

import Prelude hiding (foldr, id, length, null, reverse)
import Control.Monad.Except
import Data.Foldable (foldrM)
import Data.Sequence

import Lisp.Data
import Lisp.Monad
import Lisp.VirtualMachine

compile :: Value -> LispM (Seq Instruction)
compile ast =
  case viewl <$> toSeq ast of
    Left ([], Symbol sym) -> return [Get sym]
    Left ([], Quote lit) -> return [Push lit]
    Left ([], lit) -> return [Push lit]
    Left _ -> throwError CompileDottedList
    Right EmptyL -> return [Push Nil]
    Right (Symbol fn :< args) -> do
      result <- lookupSymbol' fn
      case result of
        Just (Macro (Left (_, native)) _) -> native args
        Just (Macro (Right compiled) scopeIDs) -> do
          let insns = Push (Lambda (Right compiled) scopeIDs) <| (fmap Push (reverse args) |> Funcall (length args))
          expanded <- eval insns
          compile expanded
        _ -> (|> Funcall (length args)) <$> ((Get fn <|) <$> compileValues args)
    Right (fn@(Cons _ _) :< args) ->
      (|> Funcall (length args)) <$> ((><) <$> compile fn <*> compileValues args)
    Right _ -> throwError $ TypeMismatch "function or macro"

compileValues :: Seq Value -> LispM (Seq Instruction)
compileValues = foldrM (\ast acc -> (acc ><) <$> compile ast) empty
