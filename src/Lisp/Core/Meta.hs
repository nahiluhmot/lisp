{-# LANGUAGE OverloadedStrings #-}

module Lisp.Core.Meta (defCoreMeta) where

import Control.Monad.Except
import Data.Foldable

import Lisp.Data
import Lisp.Monad
import Lisp.Parser
import Lisp.Compiler
import Lisp.VirtualMachine

defCoreMeta :: LispM ()
defCoreMeta = do
  defun1 "type-of" typeOf

  defun1 "read" $ \sexp ->
    case sexp of
      (String text) -> list <$> parse text
      val -> raiseTypeMismatch "string" val

  defun1 "eval" $ \sexp ->
    case toSeq sexp of
      Left _ -> raiseCompileDottedList sexp
      Right vs -> foldlM (const $ compile >=> eval) Nil vs

typeOf :: Value -> LispM Value
typeOf Nil = symbol "nil"
typeOf (Number _) = symbol "number"
typeOf (Symbol _) = symbol "symbol"
typeOf (String _) = symbol "string"
typeOf (Quote _) = symbol "quote"
typeOf (List _ _) = symbol "list"
typeOf (DottedList _ _ _) = symbol "dotted-list"
typeOf (Lambda _ _) = symbol "lambda"
typeOf (Macro _ _) = symbol "macro"
