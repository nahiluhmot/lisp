{-# LANGUAGE OverloadedStrings #-}

module Lisp.Repl where

import Prelude hiding (getLine, putStrLn, putStr)

import Control.Monad.Except
import Data.Text
import Data.Text.IO

import Lisp.Data
import Lisp.Core
import Lisp.Parser
import Lisp.Compiler
import Lisp.Prelude
import Lisp.VirtualMachine

repl :: IO ()
repl = do
  (result, _) <- runLispM (defPrelude >> (parse "(repl-main)" >>= mapM_ (compile >=> eval))) emptyLispState
  case result of
    Left err -> do
      putStrLn $ "*** unhandled error: " `append` pack (show err)
    Right () -> return ()
