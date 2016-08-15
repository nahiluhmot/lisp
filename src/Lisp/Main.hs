{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.Main where

import Prelude hiding (getLine, putStrLn, putStr)

import Data.Text
import Data.Text.IO

import Lisp.Data
import Lisp.Core
import Lisp.Prelude
import Lisp.VirtualMachine

lispMain :: IO ()
lispMain = do
  (result, _) <- runLispM (defPrelude >> funcallByName "load" [String "lisp"]) emptyLispState
  case result of
    Left err -> do
      putStrLn $ "*** unhandled error: " `append` pack (show err)
    Right _ -> return ()
