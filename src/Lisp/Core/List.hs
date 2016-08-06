{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lisp.Core.List (defCoreList) where

import Prelude hiding (length, last)
import Control.Monad.Except
import Data.Sequence
import Data.Ratio

import Lisp.Data
import Lisp.Monad

defCoreList :: LispM ()
defCoreList = do
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
      _ -> throwError $ TypeMismatch "list"

  defun2 "index" $ \vals idx ->
    case (vals, idx) of
      (List val vals', Number rational)
        | denominator rational /= 1 -> throwError $ TypeMismatch "positive integer"
        | otherwise ->
          case (fromIntegral $ numerator rational, length vals') of
            (0, _) -> return $ val
            (n, len)
              | n > len -> throwError $ IndexOutOfBounds n (succ len)
              | n < 0 -> throwError $ IndexOutOfBounds n (succ len)
              | otherwise -> return $ index vals' (pred n)
      (List _ _, _) -> throwError $ TypeMismatch "positive integer"
      _ -> throwError $ TypeMismatch "list"


  defun1 "first" $ \sexp -> do
    case sexp of
      (List x _) -> return x
      (DottedList x _ _) -> return x
      _ -> throwError $ TypeMismatch "cons"

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
      _ -> throwError $ TypeMismatch "cons"

  defun "list" $ return . list

  defun "dotted-list" $ \args -> do
    when (length args < 2) $ throwError $ ArgMismatch 2 0
    let (rest :> final) = viewr args
    return $ dottedList rest final
