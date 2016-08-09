{-# LANGUAGE OverloadedStrings #-}

module Lisp.Prelude.IO where

import Control.Monad.State.Strict
import Data.Functor
import qualified Data.Text.IO as IO
import System.IO (hFlush, stdout)

import Lisp.Data
import Lisp.Monad

defPreludeIO :: LispM ()
defPreludeIO = do
  defun1 "puts" $ \sexp -> printVal sexp $> Nil
  defun1 "print" $ \sexp ->
    case sexp of
      String str -> liftIO (IO.putStr str >> hFlush stdout) $> Nil
      _ -> raiseTypeMismatch "string" sexp
  defun0 "gets" $ String <$> liftIO IO.getLine
