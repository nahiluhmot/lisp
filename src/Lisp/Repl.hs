{-# LANGUAGE OverloadedStrings #-}

module Lisp.Repl where

import Prelude hiding (getLine, putStrLn, putStr)

import Control.Monad.Except
import qualified Data.Foldable as F
import Data.Text
import Data.Text.IO

import Lisp.Data
import Lisp.Monad
import Lisp.Parser
import Lisp.Compiler
import Lisp.Prelude
import Lisp.VirtualMachine

replSource :: Text
replSource = "\
  \(def repl (lambda ()\
    \(let ((go (lambda () (puts (eval (read (gets)))) (recur))))\
      \(puts \"Welcome to the Lisp REPL!\")\
      \(on-error (lambda (err) (puts err) (go))\
        \(go)))))\
  \(repl)\
\ "

repl :: IO ()
repl = do
  putStrLn "Welcome to the Lisp REPL!"
  (result, _) <- runLispM (defPrelude >> (parse replSource >>= mapM_ (compile >=> eval))) emptyLispState
  case result of
    Left err -> do
      putStrLn $ "*** unhandled error: " `append` pack (show err)
    Right () -> return ()
