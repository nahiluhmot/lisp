{-# LANGUAGE OverloadedStrings #-}

module Lisp.Prelude.IO where

import Control.Monad.State.Strict
import Data.Functor
import qualified Data.Text.IO as IO

import Lisp.Data
import Lisp.Monad

defPreludeIO :: LispM ()
defPreludeIO = do
  defun1 "puts" $ \sexp -> printVal sexp $> Nil
  defun0 "gets" $ String <$> liftIO IO.getLine
