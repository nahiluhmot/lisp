{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lisp.Prelude.Math (defPreludeMath) where

import Control.Monad

import Data.Ratio
import Data.Text

import Lisp.Data
import Lisp.Core

defPreludeMath :: LispM ()
defPreludeMath = do
  defBinMath "+" (+)
  defBinMath "-" (-)
  defBinMath "*" (*)
  defBinMath "/" (/)
  defBinMath' "%" $ \a b -> do
    when (denominator a /= 1) $
      raiseTypeMismatch "integer" (Number a)
    when (denominator b /= 1) $
      raiseTypeMismatch "integer" (Number b)
    return $ (mod (numerator a) (numerator b) % 1)

defBinMath :: Text -> (Rational -> Rational -> Rational) -> LispM ()
defBinMath name f = defBinMath' name (\x y -> return $ f x y)

defBinMath' :: Text -> (Rational -> Rational -> LispM Rational) -> LispM ()
defBinMath' name f =
  let go (Number x) (Number y) = Number <$> f x y
      go (Number _) val = raiseTypeMismatch "number" val
      go val _ = raiseTypeMismatch "number" val
  in  defun2 name go
