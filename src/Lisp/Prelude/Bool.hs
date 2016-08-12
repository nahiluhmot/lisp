{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lisp.Prelude.Bool (defPreludeBool) where

import Data.Text

import Lisp.Data
import Lisp.Core

defPreludeBool :: LispM ()
defPreludeBool = do
  defBinBool' "==" (==)
  defBinBool' "!=" (/=)
  defNumToBool' ">" (>)
  defNumToBool' "<" (<)
  defNumToBool' ">=" (>=)
  defNumToBool' "<=" (<=)
  defun1 "not" $ \arg -> boolToValue $ arg /= Nil

boolToValue :: Bool -> LispM Value
boolToValue True = symbol "t"
boolToValue False = return Nil

defBinBool :: Text -> (Value -> Value -> LispM Bool) -> LispM ()
defBinBool name f =
  defun2 name $ \a b -> f a b >>= boolToValue

defBinBool' :: Text -> (Value -> Value -> Bool) -> LispM ()
defBinBool' name f = defBinBool name $ \a b -> return $ f a b

defNumToBool :: Text -> (Rational -> Rational -> LispM Bool) -> LispM ()
defNumToBool name f =
  let go (Number x) (Number y) = f x y
      go (Number _) x = raiseTypeMismatch "number" x
      go x _ = raiseTypeMismatch "number" x
  in  defBinBool name go

defNumToBool' :: Text -> (Rational -> Rational -> Bool) -> LispM ()
defNumToBool' name f = defNumToBool name $ \a b -> return $ f a b
