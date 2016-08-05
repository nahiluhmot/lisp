{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Lisp.Builtins (addBuiltins) where

import Prelude hiding (id, last)
import Control.Monad.Except
import qualified Data.Foldable as F
import Data.Functor
import Data.Sequence as S
import qualified Data.Text.IO as IO

import Lisp.Data
import Lisp.Monad
import Lisp.Parser (parse)
import Lisp.VirtualMachine
import Lisp.Compiler

addBuiltins :: LispM ()
addBuiltins = do
  globalDef' "nil" Nil
  symToID "t" >>= \id -> globalDef id $ Symbol id

  lambda <- symbol "lambda"
  defmacro "lambda" $ \vals -> do
    func <- compileFunc lambda vals
    return $ [MakeLambda func]

  macro <- symbol "macro"
  defmacro "macro" $ \vals -> do
    func <- compileFunc macro vals
    return $ [MakeMacro func]

  defmacro1 "return" $ \val -> (|> Return) <$> compile' val

  defmacro2 "def" $ \name val ->
    case name of
      Symbol sym -> (|> Def sym) <$> compile' val
      _ -> throwError $ TypeMismatch "symbol"

  defmacro "if" compileIf
  defmacro "recur" compileRecur
  defmacro "let" compileLet
  defmacro1 "quote" compileQuote
  defmacro1 "syntax-quote" compileSyntaxQuote

  defun2 "+" $ \a b ->
    case (a, b) of
      (Number x, Number y) -> return . Number $ x + y
      _ -> throwError $ TypeMismatch "number"

  defun2 "-" $ \a b ->
    case (a, b) of
      (Number x, Number y) -> return . Number $ x - y
      _ -> throwError $ TypeMismatch "number"

  defun2 "*" $ \a b ->
    case (a, b) of
      (Number x, Number y) -> return . Number $ x * y
      _ -> throwError $ TypeMismatch "number"

  defun2 "/" $ \a b ->
    case (a, b) of
      (Number x, Number y) -> return . Number $ x / y
      _ -> throwError $ TypeMismatch "number"

  defun2 "==" $ \a b ->
    if a == b then symbol "t" else return Nil

  defun2 "!=" $ \a b ->
    if a == b then return Nil else symbol "t"

  defun2 ">=" $ \a b ->
    case (a, b) of
      (Number x, Number y)
        | x >= y -> symbol "t"
        | otherwise -> return Nil
      _ -> throwError $ TypeMismatch "number"

  defun2 "<=" $ \a b ->
    case (a, b) of
      (Number x, Number y)
        | x <= y -> symbol "t"
        | otherwise -> return Nil
      _ -> throwError $ TypeMismatch "number"

  defun2 ">" $ \a b ->
    case (a, b) of
      (Number x, Number y)
        | x > y -> symbol "t"
        | otherwise -> return Nil
      _ -> throwError $ TypeMismatch "number"

  defun2 "<" $ \a b ->
    case (a, b) of
      (Number x, Number y)
        | x < y -> symbol "t"
        | otherwise -> return Nil
      _ -> throwError $ TypeMismatch "number"

  defun1 "not" $ \arg ->
    if arg == Nil then symbol "t" else return Nil

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
    when (S.length args < 2) $ throwError $ ArgMismatch 2 0
    let (rest S.:> final) = viewr args
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

  defun1 "type-of" typeOf

  defun1 "puts" $ \sexp -> printVal sexp $> Nil
  defun0 "gets" $ String <$> liftIO IO.getLine

  defun1 "read" $ \sexp ->
    case sexp of
      (String text) -> list <$> parse text
      _ -> throwError $ TypeMismatch "string"

  defun1 "eval" $ \sexp ->
    case toSeq sexp of
      Left _ -> throwError CompileDottedList
      Right vs -> F.foldlM (const $ compile >=> eval) Nil vs

compileFunc :: Value -> Seq Value -> LispM CompiledFunction
compileFunc sym vals = do
  let namesToIDs = mapM unSymbol
  case viewl vals of
    EmptyL -> throwError $ ArgMismatch 1 0
    (args :< body) -> do
      (ids, extra) <-
        case toSeq args of
          (Left (xs, x)) -> (,) <$> namesToIDs xs <*> (Just <$> unSymbol x)
          (Right xs) -> (, Nothing) <$> namesToIDs xs
      insns <- compileValues body
      return $ CompiledFunction { instructions = insns
                                , argIDs = ids
                                , extraArgsID = extra
                                , source = List sym vals
                                }

compileLet :: Seq Value -> LispM (Seq Instruction)
compileLet vals = do
  when (S.null vals) $ throwError $ ArgMismatch 2 0
  let (defs :< body) = viewl vals
      go sexp =
        case toSeq sexp of
          Right [Symbol id, value] -> (|> Set id) <$> compile' value
          _ -> display sexp >>= throwError . InvalidLet
  case toSeq defs of
    Right conses -> do
      bodyInsns <- compileValues body
      (PushScope <|) <$> F.foldrM (\def acc -> (>< acc) <$> go def) (bodyInsns |> PopScope) conses
    Left _ -> display defs >>= throwError . InvalidLet

compileRecur :: Seq Value -> LispM (Seq Instruction)
compileRecur args = (|> Recur (S.length args)) <$> compileValues' args

compileIf :: Seq Value -> LispM (Seq Instruction)
compileIf vals
  | S.length vals < 2 = throwError $ ArgMismatch 2 (S.length vals)
  | otherwise = do
      let ([cond, body], rest) = S.splitAt 2 vals
      condition <- compile' cond
      thenCase <- compile body
      elseCase <- compileValues rest
      return $ (condition |> BranchUnless (2 + S.length thenCase))
            >< (thenCase |> Jump (succ $ S.length elseCase))
            >< elseCase

compileQuote :: Value -> LispM (Seq Instruction)
compileQuote val = S.singleton . Push <$> compileQuote' val

compileQuote' :: Value -> LispM Value
compileQuote' val = do
  quote <- symbol "quote"
  let go vals@(List car cdr)
        | (car /= quote) && (S.length cdr /= 1) = return vals
        | S.length cdr == 1 = Quote <$> go (S.index cdr 0)
      go curr = return curr
  go val

compileSyntaxQuote :: Value -> LispM (Seq Instruction)
compileSyntaxQuote arg = do
  unquote <- symbol "unquote"
  splat <- symbol "unquote-splat"
  cons <- symToID "cons"
  append <- symToID "append"
  let go (List car cdr)
        | car == unquote =
          case cdr of
            [val] -> Right <$> compile val
            _ -> recur
        | car == splat =
          case cdr of
            [val] -> Left <$> compile val
            _ -> recur
        | otherwise = recur
        where recur = mapM go (car <| cdr) >>= F.foldrM combine (Right [Push Nil])
      go (DottedList car cdr last) = do
        first <- mapM go (car <| cdr)
        rest <- go last
        F.foldrM combine rest first
      go val = Right <$> compileQuote val
      combine (Right car') (Right cdr') =
        return . Right $ Get cons <| ((car' >< cdr') |> Funcall 2)
      combine (Left car') (Right cdr') =
        return . Right $ Get append <| ((car' >< cdr') |> Funcall 2)
      combine _ _ =
        throwError $ InvalidSyntaxQuote "Cannot unquote-splat in cdr postion"
  result <- go arg
  case result of
    Right insns -> return insns
    Left _ -> throwError $ InvalidSyntaxQuote "Cannot unquote-splat outside of cons"

unSymbol :: Value -> LispM Int
unSymbol (Symbol id) = return id
unSymbol _ = throwError $ TypeMismatch "symbol"

typeOf :: Value -> LispM Value
typeOf Nil = symbol "nil"
typeOf (Number _) = symbol "number"
typeOf (Symbol _) = symbol "symbol"
typeOf (String _) = symbol "string"
typeOf (Quote _) = symbol "quote"
typeOf (List _ _) = symbol "list"
typeOf (DottedList _ _ _) = symbol "dotted-list"
typeOf (Lambda _ _) = symbol "lambda"
typeOf (Macro _ _) = symbol "macro"
