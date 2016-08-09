{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.Core.Error (defCoreError) where

import Prelude hiding (id)
import Data.Sequence

import Lisp.Compiler
import Lisp.Data
import Lisp.Monad

defCoreError :: LispM ()
defCoreError = do
  defmacro2 "raise" $ \val val' ->
    case (val, val') of
      (sym@(Symbol _), msg) -> do
        compiled <- compile msg
        return $ (Push sym <| (compiled |> Raise))
      (given, _) ->
        raiseTypeMismatch "symbol" given

  defmacro "on-error" $ \vals ->
    case viewl vals of
      EmptyL -> raiseArgMismatch 1 0
      (handler :< body) -> do
        handler' <- compile handler
        body' <- compileValues body
        return $ (handler' |> PushErrorHandler) >< (body' |> PopErrorHandler)

  defun1 "error-type" $ \val ->
    case val of
      (Error (LispError typ _)) -> return $ Symbol typ
      _ -> raiseTypeMismatch "error" val

  defun1 "error-message" $ \val ->
    case val of
      (Error (LispError _ msg)) -> return $ String msg
      _ -> raiseTypeMismatch "error" val
