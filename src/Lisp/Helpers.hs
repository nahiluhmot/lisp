{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lisp.Helpers where

import Prelude hiding (id)
import Control.Monad.Except
import qualified Data.Foldable as F
import Data.Monoid
import Data.Ratio
import Data.Sequence
import Numeric (fromRat)

import Data.Text
import Data.Text.IO as IO

import Lisp.Data
import Lisp.Monad

printVal :: Value -> LispM ()
printVal = liftIO . IO.putStrLn <=< display

display :: Value -> LispM Text
display Nil = return "()"
display (Number val)
  | denominator val == 1 = return . pack . show $ numerator val
  | otherwise = return . pack . show $ (fromRat val :: Double)
display (Symbol id) = idToSym id
display (String str)  = return $ "\"" <> str <> "\""
display (Quote val)  = (mappend "'") <$> display val
display (List x xs)  = do
  texts <- mapM display . F.toList $ x <| xs
  return $ "(" <> intercalate " " texts <> ")"
display (DottedList x xs y) = do
  texts <- mapM display . F.toList $ x <| xs
  text <- display y
  return $ "(" <> intercalate " " texts <> " . " <> text <> ")"
display (Lambda (Left (n, _)) _) = do
  return $ "#<native function: " <> n <> ">"
display (Lambda (Right (CompiledFunction _ _ _ src)) _) = do
  displayed <- display src
  return $ "#<" <> displayed <> ">"
display (Macro (Left (n, _)) _) = do
  return $ "#<native macro: " <> n <> ">"
display (Macro (Right (CompiledFunction _ _ _ src)) _) = do
  displayed <- display src
  return $ "#<" <> displayed <> ">"
