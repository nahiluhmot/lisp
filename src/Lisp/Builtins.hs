{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Lisp.Builtins (addBuiltins) where

import Prelude hiding (id)
import Control.Monad.Except
import qualified Data.Foldable as F
import Data.Functor
import Data.Sequence as S
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
    (|> Return) <$> compile' (index vals 0)

  defmacro "def" compileDef
  defmacro "if" compileIf
  defmacro "recur" compileRecur
  defmacro "let" compileLet
  defmacro "quote" compileQuote
  defmacro "syntax-quote" compileSyntaxQuote

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
  defun "dotted-list" $ \args -> do
    when (S.length args < 2) $ throwError $ ArgMismatch 2 0
    let (rest S.:> final) = viewr args
    return $ foldr Cons final rest
  defunN 2 "append" $ \[first, rest] ->
    case toSeq first of
      Right vals -> return $ foldr Cons rest vals
      Left _ -> throwError $ TypeMismatch "list"

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
      Right list -> F.foldlM (const $ compile >=> eval) Nil list

defmacro :: T.Text -> (Seq Value -> LispM (Seq Instruction)) -> LispM ()
defmacro sym func = globalDef' sym $ Macro (Left (sym, func)) []

defun :: T.Text -> (Seq Value -> LispM Value) -> LispM ()
defun sym func = globalDef' sym $ Lambda (Left (sym, func)) []

defun0 :: T.Text -> LispM Value -> LispM ()
defun0 sym func =
  defun sym $ \args -> do
    when (not $ S.null args) $ throwError $ ArgMismatch 0 (S.length args)
    func

defun1 :: T.Text -> (Value -> LispM Value) -> LispM ()
defun1 sym func = defunN 1 sym $ func . flip index 0

defunN :: Int -> T.Text -> (Seq Value -> LispM Value) -> LispM ()
defunN n sym func =
  defun sym $ \args -> do
    when (S.length args /= n) $ throwError $ ArgMismatch n (S.length args)
    func args

compileFunc :: Value -> Seq Value -> LispM CompiledFunction
compileFunc sym list = do
  let namesToIDs = mapM unSymbol
  case viewl list of
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
                                , source = Cons sym $ foldr Cons Nil list
                                }

compileLet :: Seq Value -> LispM (Seq Instruction)
compileLet list = do
  when (S.null list) $ throwError $ ArgMismatch 2 0
  let (defs :< body) = viewl list
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

compileDef :: Seq Value -> LispM (Seq Instruction)
compileDef [Symbol sym, sexp] = (|> Def sym) <$> compile' sexp
compileDef [_, _] = throwError $ TypeMismatch "symbol"
compileDef vals = throwError $ ArgMismatch 2 (S.length vals)

compileIf :: Seq Value -> LispM (Seq Instruction)
compileIf list
  | S.length list < 2 = throwError $ ArgMismatch 2 (S.length list)
  | otherwise = do
      let ([cond, body], rest) = S.splitAt 2 list
      condition <- compile' cond
      thenCase <- compile body
      elseCase <- compileValues rest
      return $ (condition |> BranchUnless (2 + S.length thenCase))
            >< (thenCase |> Jump (succ $ S.length elseCase))
            >< elseCase

compileQuote :: Seq Value -> LispM (Seq Instruction)
compileQuote [val] = (S.singleton . Push) <$> compileQuote' val
compileQuote list = throwError $ ArgMismatch 1 (S.length list)

compileQuote' :: Value -> LispM Value
compileQuote' val = do
  quote <- symbol "quote"
  let go cons@(Cons car cdr)
        | car /= quote = return cons
        | otherwise =
          case toSeq cdr of
            Right [curr] -> Quote <$> go curr
            _ -> return cons
      go curr = return curr
  go val

compileSyntaxQuote :: Seq Value -> LispM (Seq Instruction)
compileSyntaxQuote [arg] = do
  unquote <- symbol "unquote"
  splat <- symbol "unquote-splat"
  cons <- symToID "cons"
  append <- symToID "append"
  let go (Cons car cdr)
        | car == unquote =
          case toSeq cdr of
            Right [val] -> Right <$> compile val
            _ -> recur
        | car == splat =
          case toSeq cdr of
            Right [val] -> Left <$> compile val
            _ -> recur
        | otherwise = recur
        where recur = do
                result <- go car
                result' <- go cdr
                case (result, result') of
                  (Right car', Right cdr') ->
                    return . Right $ Get cons <| ((car' >< cdr') |> Funcall 2)
                  (Left car', Right cdr') ->
                    return . Right $ Get append <| ((car' >< cdr') |> Funcall 2)
                  (_, Left _) -> throwError $ InvalidSyntaxQuote "Cannot unquote-splat in cdr postion"
      go val = Right <$> compileQuote [val]
  result <- go arg
  case result of
    Right insns -> return insns
    Left _ -> throwError $ InvalidSyntaxQuote "Cannot unquote-splat outside of cons"
compileSyntaxQuote list = throwError $ ArgMismatch 1 (S.length list)

unSymbol :: Value -> LispM Int
unSymbol (Symbol id) = return id
unSymbol _ = throwError $ TypeMismatch "symbol"

typeOf :: Value -> LispM Value
typeOf Nil = symbol "nil"
typeOf (Number _) = symbol "number"
typeOf (Symbol _) = symbol "symbol"
typeOf (String _) = symbol "string"
typeOf (Quote _) = symbol "quote"
typeOf (Cons _ _) = symbol "cons"
typeOf (Lambda _ _) = symbol "lambda"
typeOf (Macro _ _) = symbol "macro"
