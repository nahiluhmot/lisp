{-# LANGUAGE OverloadedStrings #-}

module Lisp.Errors where

import Control.Monad.Except
import Prelude hiding (id)
import Data.Text

import Lisp.Data

raise :: Int -> Text -> LispM ()
raise id msg = throwError $ LispError id msg

raise' :: Text -> Text -> LispM ()
raise' sym msg = symToID sym >>= \id -> throwError $ LispError id msg

raiseInternal :: Text -> LispM ()
raiseInternal = raise' "internal-error"

raiseFullSymbolTable :: LispM ()
raiseFullSymbolTable = raiseInternal "Cannot create a new symbol"

raiseUnsetSymbol :: Int -> LispM ()
raiseUnsetSymbol id = raise "unset-symbol" $ "Cannot find symbol with ID: " <> pack (show id)

raiseUndefinedValue :: Text -> LispM ()
raiseUndefinedValue name =
  raise "undefined-value" $ "`" <> name <> "` is not in scope"

raiseEmptyStack :: LispM ()
raiseEmptyStack =
  raise "empty-stack" $ "Cannot pop off on"
