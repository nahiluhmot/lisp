{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Lisp.Compiler (compile, compileValues) where

import Prelude hiding (foldr, id)
import Control.Monad.State hiding (state)
import Control.Monad.Except
import Data.Foldable (foldrM)
import qualified Data.Sequence as S

import Lisp.Data
import Lisp.Monad
import Lisp.VirtualMachine

compile :: Value -> LispM (S.Seq Instruction)
compile ast =
  case toSeq ast of
    Left (list, literal) -> do
      unless (S.null list) $ throwError CompileDottedList
      case literal of
        Symbol sym -> return $ S.singleton (Get sym)
        Quote lit -> return $ S.singleton (Push lit)
        lit -> return $ S.singleton (Push lit)
    Right list ->
      case S.viewl list of
        S.EmptyL -> return $ S.singleton (Push Nil)
        (Symbol fn S.:< args) -> do
          result' <- fmap Just (lookupSymbol fn) `catchError` const (return Nothing)
          case result' of
            Just (Macro (Left (_, native)) _) -> native args
            Just (Macro (Right compiled) scopeIDs) -> do
              let insns = Push (Lambda (Right compiled) scopeIDs) S.<| (fmap Push (S.reverse args) S.|> Funcall (S.length args))
              expanded <- eval insns
              compile expanded
            _ -> (S.|> Funcall (S.length args)) <$> ((Get fn S.<|) <$> compileValues args)
        (fn@(Cons _ _) S.:< args) ->
          (S.|> Funcall (S.length args)) <$> ((S.><) <$> compile fn <*> compileValues args)
        _ -> throwError $ TypeMismatch "symbol"

compileValues :: S.Seq Value -> LispM (S.Seq Instruction)
compileValues = foldrM (\ast acc -> (acc S.><) <$> compile ast) S.empty
