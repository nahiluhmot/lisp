{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lisp.Core.Math (defCoreMath) where

import Data.Text

import Lisp.Data
import Lisp.Monad

defCoreMath :: LispM ()
defCoreMath = do
  defBinMath "+" (+)
  defBinMath "-" (-)
  defBinMath "*" (*)
  defBinMath "/" (/)

defBinMath :: Text -> (Rational -> Rational -> Rational) -> LispM ()
defBinMath name f =
  let go (Number x) (Number y) = return . Number $ f x y
      go (Number _) val = raiseTypeMismatch "number" val
      go val _ = raiseTypeMismatch "number" val
  in  defun2 name go
