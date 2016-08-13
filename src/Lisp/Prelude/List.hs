{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lisp.Prelude.List (defPreludeList) where

import Control.Monad
import Prelude hiding (length, last)
import Data.Sequence as S
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

  defun2 "snoc" $ \a b ->
    case a of
      Nil -> return $ List b []
      List first rest -> return $ list (first <| (rest |> b))
      _ -> raiseTypeMismatch "list" a

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

  defun1 "last" $ \sexp ->
    case sexp of
      List val vals ->
        case viewr vals of
          EmptyR -> return val
          (_ :> val') -> return val'
      _ -> raiseTypeMismatch "list" sexp

  defun1 "but-last" $ \sexp ->
    case sexp of
      List val vals ->
        case viewr vals of
          EmptyR -> return Nil
          (vals' :> _) -> return $ List val vals'
      _ -> raiseTypeMismatch "list" sexp

  defun1 "length" $ \val ->
    case val of
      Nil -> return $ Number 0
      List _ vals -> return . Number . fromIntegral . succ $ length vals
      _ -> raiseTypeMismatch "list" val

  defun2 "append" $ \first rest ->
    case (first, rest) of
      (List val vals, List val' vals') ->
        return $ List val (vals >< (val' <| vals'))
      (List val vals, DottedList val' vals' last) ->
        return $ DottedList val (vals >< (val' <| vals')) last
      (List val vals, Nil) -> return $ List val vals
      (List val vals, val') -> return $ DottedList val vals val'
      (String str, String str') -> return . String $ mappend str str'
      (val, _) -> raiseTypeMismatch "list" val

  defun2 "index" $ listWithPositiveInteger $ \idx vals -> do
    let len = length vals
    when (idx >= len) $
      raiseIndexOutOfBounds idx len
    return $ index vals idx

  defun2 "take" $ listWithPositiveInteger $ \idx vals ->
    return . list $ S.take idx vals

  defun2 "drop" $ listWithPositiveInteger $ \idx vals ->
    return . list $ S.drop idx vals

  defun2 "split-at" $ listWithPositiveInteger $ \idx vals ->
    let (first, rest) = S.splitAt idx vals
    in  return $ DottedList (list first) [] (list rest)

  defun "list" $ return . list

  defun "dotted-list" $ \args -> do
    when (length args < 2) $ raiseArgMismatch 2 0
    let (rest :> final) = viewr args
    return $ dottedList rest final

listWithPositiveInteger :: (Int -> Seq Value -> LispM Value) -> Value -> Value -> LispM Value
listWithPositiveInteger f num@(Number idx) vals
  | denominator idx /= 1 = raiseTypeMismatch "positive integer" num
  | otherwise =
    let n = fromIntegral $ numerator idx
    in  case vals of
          Nil -> f n []
          (List val vals') -> f n (val <| vals')
          _ -> raiseTypeMismatch "list" vals
listWithPositiveInteger _ _ num = raiseTypeMismatch "positive integer" num
