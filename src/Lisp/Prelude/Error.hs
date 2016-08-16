{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.Prelude.Error (defPreludeError) where

import Prelude hiding (id, length)
import Control.Monad.Except
import Data.Monoid
import Data.Sequence
import qualified Data.Vector as V

import Lisp.Compiler
import Lisp.Data
import Lisp.Core

defPreludeError :: LispM ()
defPreludeError = do
  defun "raise" $ \vals ->
    case length vals of
      1 ->
        case index vals 0 of
          Symbol sym -> raise' sym ""
          String str -> raise "runtime-error" str
          Error err -> throwError err
          val -> raiseTypeMismatch "symbol | string | error" val
      2 ->
        case (index vals 0, index vals 1) of
          (Symbol sym, String msg) -> raise' sym msg
          (Symbol _, val) -> raiseTypeMismatch "string" val
          (val, _) -> raiseTypeMismatch "symbol" val
      n -> raiseArgMismatch 1 n

  defmacro "on-error" $ \vals ->
    case viewl vals of
      EmptyL -> raiseArgMismatch 1 0
      (handler :< body) -> do
        handler' <- compile handler
        body' <- compileValues body
        return $ (V.snoc handler' PushErrorHandler) <> (V.snoc body' PopErrorHandler)

  defun1 "error-type" $ \val ->
    case val of
      (Error (LispError typ _)) -> return $ Symbol typ
      _ -> raiseTypeMismatch "error" val

  defun1 "error-message" $ \val ->
    case val of
      (Error (LispError _ msg)) -> return $ String msg
      _ -> raiseTypeMismatch "error" val
