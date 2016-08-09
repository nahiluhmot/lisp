{-# LANGUAGE OverloadedStrings #-}

module Lisp.Prelude.Constants (defPreludeConstants) where

import Prelude hiding (id)
import Lisp.Data
import Lisp.Monad

defPreludeConstants :: LispM ()
defPreludeConstants = do
  globalDef' "nil" Nil
  symToID "t" >>= \id -> globalDef id $ Symbol id
