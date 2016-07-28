{-# LANGUAGE OverloadedStrings #-}

module Lisp.Compiler ( compile
                     , compileValues
                     ) where

import Prelude hiding (id)
import Control.Monad
import Control.Monad.Except
import qualified Data.Foldable as F
import qualified Data.Sequence as S

import Lisp.Data
import Lisp.Monad

compile :: Value -> LispM (S.Seq Instruction)
compile = compileValue

compileValues :: S.Seq Value -> LispM (S.Seq Instruction)
compileValues = F.foldrM (\ast acc -> (acc S.><) <$> compileValue ast) S.empty

compileValue :: Value -> LispM (S.Seq Instruction)
compileValue ast = do
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
          result <- lookupBuiltin fn
          case result of
            Just func -> func args
            Nothing ->
              (S.|> Funcall (S.length args)) <$> ((Get fn S.<|) <$> compileValues args)
        (fn@(Cons _ _) S.:< args) ->
          (S.|> Funcall (S.length args)) <$> ((S.><) <$> compileValue fn <*> compileValues args)
        _ -> throwError $ TypeMismatch "symbol"
