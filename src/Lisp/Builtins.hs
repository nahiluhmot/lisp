{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Lisp.Builtins (addBuiltins) where

import Prelude hiding (id)
import Control.Monad.Except
import qualified Data.Foldable as F
import Data.Functor
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as IO

import Lisp.Data
import Lisp.Monad
import Lisp.Parser (parse)
import Lisp.VirtualMachine
import Lisp.Compiler
import Lisp.Helpers

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

  defmacro "return" $ \vals -> do
    let argc = S.length vals
    when (argc /= 1) $
      throwError $ ArgMismatch 1 argc
    (S.|> Return) <$> compile (S.index vals 0)

  defmacro "def" compileDef
  defmacro "if" compileIf
  defmacro "recur" compileRecur
  defmacro "let" compileLet

  defunN 2 "+" $ \[a, b] ->
    case (a, b) of
      (Number x, Number y) -> return . Number $ x + y
      _ -> throwError $ TypeMismatch "number"

  defunN 2 "-" $ \[a, b] ->
    case (a, b) of
      (Number x, Number y) -> return . Number $ x - y
      _ -> throwError $ TypeMismatch "number"

  defunN 2 "*" $ \[a, b] ->
    case (a, b) of
      (Number x, Number y) -> return . Number $ x * y
      _ -> throwError $ TypeMismatch "number"

  defunN 2 "/" $ \[a, b] ->
    case (a, b) of
      (Number x, Number y) -> return . Number $ x / y
      _ -> throwError $ TypeMismatch "number"

  defunN 2 "eq" $ \[a, b] ->
    if a == b then symbol "t" else return Nil

  defunN 2 "neq" $ \[a, b] ->
    if a == b then return Nil else symbol "t"

  defun1 "not" $ \arg ->
    if arg == Nil then symbol "t" else return Nil

  defunN 2 "cons" $ \[a, b] -> return $ Cons a b

  defun1 "car" $ \sexp ->
    case sexp of
      (Cons car _) -> return car
      _ -> throwError $ TypeMismatch "cons"

  defun1 "cdr" $ \sexp ->
    case sexp of
      (Cons _ cdr) -> return cdr
      _ -> throwError $ TypeMismatch "cons"

  defun "list" $ return . foldr Cons Nil

  defun1 "type-of" typeOf

  defun1 "puts" $ \sexp -> printVal sexp $> Nil
  defun0 "gets" $ String <$> liftIO IO.getLine

  defun1 "read" $ \sexp ->
    case sexp of
      (String text) -> foldr Cons Nil <$> parse text
      _ -> throwError $ TypeMismatch "string"

  defun1 "eval" $ \sexp ->
    case toSeq sexp of
      Left _ -> throwError CompileDottedList
      Right list -> do
        result <- S.viewr <$> mapM (compile >=> eval) list
        case result of
          S.EmptyR -> return Nil
          (_ S.:> x) -> return x

defmacro :: T.Text -> (S.Seq Value -> LispM (S.Seq Instruction)) -> LispM ()
defmacro sym func = globalDef' sym $ Macro (Left (sym, func)) []

defun :: T.Text -> (S.Seq Value -> LispM Value) -> LispM ()
defun sym func = globalDef' sym $ Lambda (Left (sym, func)) []

defun0 :: T.Text -> LispM Value -> LispM ()
defun0 sym func =
  defun sym $ \args -> do
    when (not $ S.null args) $ throwError $ ArgMismatch 0 (S.length args)
    func

defun1 :: T.Text -> (Value -> LispM Value) -> LispM ()
defun1 sym func =
  defun sym $ \args -> do
    when (S.length args /= 1) $ throwError $ ArgMismatch 1 (S.length args)
    func (S.index args 0)

defunN :: Int -> T.Text -> (S.Seq Value -> LispM Value) -> LispM ()
defunN n sym func =
  defun sym $ \args -> do
    when (S.length args /= n) $ throwError $ ArgMismatch n (S.length args)
    func args

compileFunc :: Value -> S.Seq Value -> LispM CompiledFunction
compileFunc sym list = do
  let namesToIDs = F.foldlM (\acc x -> (acc S.|>) <$> toSymbolID x) S.empty
  case S.viewl list of
    S.EmptyL -> throwError $ ArgMismatch 1 0
    (args S.:< body) -> do
      (ids, extra) <-
        case toSeq args of
          (Left (xs, x)) -> (,) <$> namesToIDs xs <*> (Just <$> toSymbolID x)
          (Right xs) -> (, Nothing) <$> namesToIDs xs
      insns <- compileValues (S.reverse body)
      return $ CompiledFunction { instructions = insns
                                , argIDs = ids
                                , extraArgsID = extra
                                , source = Cons sym $ foldr Cons Nil list
                                }

compileLet :: S.Seq Value -> LispM (S.Seq Instruction)
compileLet list = do
  when (S.null list) $ throwError $ ArgMismatch 2 0
  let (defs S.:< body) = S.viewl list
      go sexp =
        case toSeq sexp of
          Right [Symbol id, value] -> (S.|> Set id) <$> compile value
          _ -> display sexp >>= throwError . InvalidLet
  case toSeq defs of
    Right conses -> do
      bodyInsns <- compileValues body
      (PushScope S.<|) <$> F.foldrM (\def acc -> (S.>< acc) <$> go def) (bodyInsns S.|> PopScope) conses
    Left _ -> display defs >>= throwError . InvalidLet

compileRecur :: S.Seq Value -> LispM (S.Seq Instruction)
compileRecur args = (S.|> Recur (S.length args)) <$> compileValues args

compileDef :: S.Seq Value -> LispM (S.Seq Instruction)
compileDef [Symbol sym, sexp] = (S.|> Def sym) <$> compile sexp
compileDef [_, _] = throwError $ TypeMismatch "symbol"
compileDef vals = throwError $ ArgMismatch 2 (length vals)

compileIf :: S.Seq Value -> LispM (S.Seq Instruction)
compileIf list
  | S.length list < 2 = throwError $ ArgMismatch 2 (S.length list)
  | otherwise = do
      let ([cond, body], rest) = S.splitAt 2 list
      cond' <- compile cond
      body' <- compile body
      rest' <- compileValues rest
      if null rest' then
        return $ cond' S.>< (BranchUnless (2 + S.length body') S.<| body')
      else
        return $ cond' S.>< ((BranchUnless (2 + S.length body') S.<| body') S.>< (Jump (2 + S.length rest') S.<| rest'))

toSymbolID :: Value -> LispM Int
toSymbolID (Symbol id) = return id
toSymbolID _ = throwError $ TypeMismatch "symbol"

typeOf :: Value -> LispM Value
typeOf Nil = symbol "nil"
typeOf (Number _) = symbol "number"
typeOf (Symbol _) = symbol "symbol"
typeOf (String _) = symbol "string"
typeOf (Quote _) = symbol "quote"
typeOf (Cons _ _) = symbol "cons"
typeOf (Lambda _ _) = symbol "lambda"
typeOf (Macro _ _) = symbol "macro"
