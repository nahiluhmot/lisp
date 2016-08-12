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

replSource :: Text
replSource = "\
  \(def repl (lambda ()\
    \(let ((go (lambda ()\
                 \(print \"> \")\
                 \(puts (eval (read (gets))))\
                 \(recur))))\
      \(on-error (lambda (err) (puts err) (repl))\
        \(go)))))\
  \(puts \"Welcome to the Lisp REPL!\")\
  \(repl)\
\ "

repl :: IO ()
repl = do
  (result, _) <- runLispM (defPrelude >> (parse replSource >>= mapM_ (compile >=> eval))) emptyLispState
  case result of
    Left err -> do
      putStrLn $ "*** unhandled error: " `append` pack (show err)
    Right () -> return ()
