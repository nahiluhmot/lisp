{-# LANGUAGE OverloadedStrings #-}

module Lisp.Prelude.IO where

import Control.Monad.State.Strict
import Data.Functor
import Data.Sequence
import qualified Data.Text.IO as IO
import qualified Data.Text as T
import System.IO (hFlush, stdout)
import System.Environment

import Lisp.Data
import Lisp.Core

defPreludeIO :: LispM ()
defPreludeIO = do
  defun1 "puts" $ \sexp -> printVal sexp $> Nil
  defun1 "print" $ \sexp ->
    case sexp of
      String str -> liftIO (IO.putStr str >> hFlush stdout) $> Nil
      _ -> raiseTypeMismatch "string" sexp
  defun0 "gets" $ String <$> liftIO IO.getLine

  rawArgv <- liftIO getArgs
  def' "*argv*" $ list . fromList $ map (String . T.pack) rawArgv

  defun1 "get-env" $ \k ->
    case k of
      (String key) -> maybe Nil (String . T.pack) <$> liftIO (lookupEnv (T.unpack key))
      _ -> raiseTypeMismatch "string" k

  defun2 "set-env" $ \k v ->
    case (k, v) of
      (String key, String val) -> liftIO (setEnv (T.unpack key) (T.unpack val)) $> Nil
      (String _, val) -> raiseTypeMismatch "string" val
      (key, _) -> raiseTypeMismatch "string" key
