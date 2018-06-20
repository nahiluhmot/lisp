{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.Main where

import Prelude hiding (getLine, putStrLn, putStr)

import Data.Monoid ((<>))
import Data.Text
import Data.Text.IO

import Lisp.Data
import Lisp.Core
import Lisp.Prelude
import Lisp.VirtualMachine
import qualified Lisp.SymbolTable as ST

lispMain :: IO ()
lispMain = do
  (result, state) <- runLispM (defPrelude >> funcallByName "load" [String "lisp"]) emptyLispState
  case result of
    Left err@(LispError sym msg) ->
      case ST.idToSym sym (symbolTable state) of
        Nothing -> putStrLn $ "*** unhandled error: " <> pack (show err)
        Just typ -> putStrLn $ "*** unhandled error of type " <> typ <> ": " <> msg
    Right _ -> return ()
