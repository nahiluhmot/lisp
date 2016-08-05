{-# LANGUAGE OverloadedStrings #-}

module Lisp.Core.Constants (defCoreConstants) where

import Lisp.Data
import Lisp.Monad

defCoreConstants :: LispM ()
defCoreConstants = do
  globalDef' "nil" Nil
  symToID "t" >>= \id -> globalDef id $ Symbol id
