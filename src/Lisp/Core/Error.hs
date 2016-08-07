{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.Core.Error (defCoreError) where

import Prelude hiding (id)
import Data.Sequence

import Lisp.Compiler
import Lisp.Data
import Lisp.Monad

defCoreError :: LispM ()
defCoreError = do
  defmacro2 "raise" $ \val val' ->
    case (val, val') of
      (sym@(Symbol _), msg) -> do
        compiled <- compile msg
        return $ (Push sym <| (compiled |> Raise))
      (given, _) ->
        raiseTypeMismatch "symbol" given
