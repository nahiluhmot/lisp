{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lisp.Prelude.List (defPreludeList) where

import Control.Monad
import Prelude hiding (length, last)
import Data.Sequence
import Data.Ratio

import Lisp.Data
import Lisp.Core

defPreludeList :: LispM ()
defPreludeList = do
  defun2 "cons" $ \a b ->
    case b of
      Nil -> return $ List a []
      List first rest -> return $ list (a <| first <| rest)
      DottedList first rest final -> return $ dottedList (a <| first <| rest) final
      _ -> return $ DottedList a [] b

  defun2 "append" $ \first rest ->
    case (first, rest) of
      (List val vals, List val' vals') ->
        return $ List val (vals >< (val' <| vals'))
      (List val vals, DottedList val' vals' last) ->
        return $ DottedList val (vals >< (val' <| vals')) last
      (List val vals, Nil) -> return $ List val vals
      (List val vals, val') -> return $ DottedList val vals val'
      (val, _) -> raiseTypeMismatch "list" val

  defun1 "length" $ \val ->
    case val of
      Nil -> return $ Number 0
      List _ vals -> return . Number . fromIntegral . succ $ length vals
      _ -> raiseTypeMismatch "list" val

  defun2 "index" $ \vals idx ->
    case (vals, idx) of
      (List val vals', num@(Number rational))
        | denominator rational /= 1 -> raiseTypeMismatch "positive integer" num
        | otherwise ->
          case (fromIntegral $ numerator rational, length vals') of
            (0, _) -> return $ val
            (n, len)
              | n > len -> raiseIndexOutOfBounds n len
              | n < 0 -> raiseIndexOutOfBounds n len
              | otherwise -> return $ index vals' (pred n)
      (List _ _, val) -> raiseTypeMismatch "positive integer" val
      (val, _) -> raiseTypeMismatch "list" val

  defun1 "first" $ \sexp -> do
    case sexp of
      (List x _) -> return x
      (DottedList x _ _) -> return x
      val -> raiseTypeMismatch "cons" val

  defun1 "rest" $ \sexp ->
    case sexp of
      (List _ xs) ->
        case viewl xs of
          EmptyL -> return Nil
          (first :< rest) -> return $ List first rest
      (DottedList _ xs x) ->
        case viewl xs of
          EmptyL -> return x
          (first :< rest) -> return $ DottedList first rest x
      val -> raiseTypeMismatch "cons" val

  defun "list" $ return . list

  defun "dotted-list" $ \args -> do
    when (length args < 2) $ raiseArgMismatch 2 0
    let (rest :> final) = viewr args
    return $ dottedList rest final
