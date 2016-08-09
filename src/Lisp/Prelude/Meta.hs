{-# LANGUAGE OverloadedStrings #-}

module Lisp.Prelude.Meta (defPreludeMeta) where

import Control.Monad.Except
import Data.Foldable
import Data.Sequence as S

import Lisp.Data
import Lisp.Monad
import Lisp.Parser
import Lisp.Compiler
import Lisp.VirtualMachine

defPreludeMeta :: LispM ()
defPreludeMeta = do
  defun1 "type-of" typeOf

  defun1 "intern" $ \val ->
    case val of
      String str -> symbol str
      _ -> raiseTypeMismatch "string" val

  defun1 "to-s" $ \val -> String <$> display val

  defun1 "read" $ \sexp ->
    case sexp of
      (String text) -> list <$> parse text
      val -> raiseTypeMismatch "string" val

  defun1 "eval" $ \sexp ->
    case toSeq sexp of
      Left _ -> raiseCompileDottedList sexp
      Right vs -> foldlM (const $ compile >=> eval) Nil vs

  defun "id" $ \sexp ->
    case viewl sexp of
      EmptyL -> return Nil
      (car :< cdr)
        | S.null cdr -> return car
        | otherwise -> return $ List car cdr

typeOf :: Value -> LispM Value
typeOf Nil = symbol "nil"
typeOf (Number _) = symbol "number"
typeOf (Symbol _) = symbol "symbol"
typeOf (String _) = symbol "string"
typeOf (Quote _) = symbol "quote"
typeOf (List _ _) = symbol "list"
typeOf (DottedList _ _ _) = symbol "dotted-list"
typeOf (Lambda _) = symbol "lambda"
typeOf (Macro _) = symbol "macro"
typeOf (Error _) = symbol "error"
