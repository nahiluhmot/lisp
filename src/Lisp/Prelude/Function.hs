{-# LANGUAGE OverloadedStrings #-}

module Lisp.Prelude.Function (defPreludeFunction) where

import Prelude hiding (id)
import Data.Sequence as S

import Lisp.Data
import Lisp.Monad
import Lisp.VirtualMachine

defPreludeFunction :: LispM ()
defPreludeFunction = do
  defun "funcall" $ \vals ->
    case viewl vals of
      EmptyL -> raiseArgMismatch 1 0
      (Lambda func :< args) -> funcall func args
      (val :< _) -> raiseTypeMismatch "lambda" val

  defun2 "apply" $ \fn args ->
    case (fn, args) of
      (Lambda func, List car cdr) -> funcall func (car <| cdr)
      (Lambda func, Nil) -> funcall func empty
      (Lambda _, val) -> raiseTypeMismatch "list" val
      (val, _) -> raiseTypeMismatch "lambda" val
