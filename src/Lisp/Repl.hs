{-# LANGUAGE OverloadedStrings #-}

module Lisp.Repl where

import Prelude hiding (getLine, putStrLn, putStr)

import Control.Monad.State hiding (state)
import qualified Data.Foldable as F
import Data.Text
import Data.Text.IO
import System.IO (hFlush, stdout)

import Lisp.Data
import Lisp.Monad
import Lisp.Parser
import Lisp.Compiler
import Lisp.VirtualMachine
import Lisp.Builtins

repl :: IO ()
repl =
  let go state = do
        (result, state') <- flip runLispM state $ do
          liftIO (putStr "> " >> hFlush stdout >> getLine) >>=
            parse >>=
            F.foldlM (\_ sexp -> compile sexp >>= eval) Nil
            >>= printVal
        case result of
          Left err -> putStrLn $ "*** error: " `append` pack (show err)
          Right _ -> return ()
        go state'
      welcome = putStrLn "Welcome to the Lisp REPL!"
  in  do
    welcome
    (result, state) <- runLispM addBuiltins emptyLispState
    case result of
      Left err -> do
        putStrLn $ "Error adding builtins to language: " `append` pack (show err)
      Right _ -> go state
