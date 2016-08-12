{-# LANGUAGE OverloadedStrings #-}

module Lisp.Prelude.Constants (defPreludeConstants) where

import Prelude hiding (id)
import Lisp.Data
import Lisp.Core

defPreludeConstants :: LispM ()
defPreludeConstants = do
  def' "nil" Nil
  symToID "t" >>= \id -> def id $ Symbol id
