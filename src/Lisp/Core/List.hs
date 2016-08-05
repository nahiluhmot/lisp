{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Lisp.Core.List (defCoreList) where

import Prelude hiding (length, last)
import Control.Monad.Except
import Data.Sequence

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

  defun2 "append" $ \first rest ->
    case (first, rest) of
      (List val vals, List val' vals') ->
        return $ List val (vals >< (val' <| vals'))
      (List val vals, DottedList val' vals' last) ->
        return $ DottedList val (vals >< (val' <| vals')) last
      (List val vals, Nil) -> return $ List val vals
      (List val vals, val') -> return $ DottedList val vals val'
      _ -> throwError $ TypeMismatch "list"
