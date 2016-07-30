{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Lisp.Builtins where

import Prelude hiding (id)
import Control.Monad.Except
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Text as T
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

  addBuiltin "lambda" compileLambda
  addBuiltin "macro" compileMacro
  addBuiltin "def" compileDef
  addBuiltin "if" compileIf
  addBuiltin "recur" compileRecur
  addBuiltin "let" compileLet

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
    if eq a b then symbol "t" else return Nil

  defunN 2 "neq" $ \[a, b] ->
    if eq a b then return Nil else symbol "t"

  defun1 "not" $ \arg ->
    if eq arg Nil then symbol "t" else return Nil

  defunN 2 "cons" $ \[a, b] -> return $ Cons a b

  defun1 "car" $ \sexp ->
    case sexp of
      (Cons car _) -> return car
      _ -> throwError $ TypeMismatch "cons"

  defun1 "cdr" $ \sexp ->
    case sexp of
      (Cons _ cdr) -> return cdr
      _ -> throwError $ TypeMismatch "cons"

  defun1 "type-of" typeOf

  defun1 "puts" $ \sexp -> display sexp >>= liftIO . IO.putStrLn >> return Nil
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

compileLambda :: S.Seq Value -> LispM (S.Seq Instruction)
compileLambda vals = symbol "lambda" >>= \id -> compileFunc MakeLambda id vals

compileMacro :: S.Seq Value -> LispM (S.Seq Instruction)
compileMacro vals = symbol "macro" >>= \id -> compileFunc MakeMacro id vals

defun :: T.Text -> (S.Seq Value -> LispM Value) -> LispM ()
defun sym func =
  globalDef' sym $ Lambda (Left (NativeFunction sym func)) []

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

compileFunc :: (Function -> Instruction) -> Value -> S.Seq Value -> LispM (S.Seq Instruction)
compileFunc toInsn name list =
  let namesToIDs = F.foldlM (\acc x -> (acc S.|>) <$> toSymbolID x) S.empty
  in  case S.viewl list of
        S.EmptyL -> throwError $ ArgMismatch 1 0
        (args S.:< body) -> do
          (ids, extra) <-
            case toSeq args of
              (Left (xs, x)) -> (,) <$> namesToIDs xs <*> (Just <$> toSymbolID x)
              (Right xs) -> (, Nothing) <$> namesToIDs xs
          insns <- compileValues (S.reverse body)
          let compiled = CompiledFunction { instructions = insns
                                          , argIDs = ids
                                          , extraArgsID = extra
                                          , source = Cons name $ foldr Cons Nil list
                                          }
          return [toInsn $ Right compiled]

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

defFunctionLikeInstruction :: T.Text -> Instruction -> Int -> LispM ()
defFunctionLikeInstruction name insn argc = do
  let args = [0 .. pred argc]
      func = CompiledFunction { instructions = F.foldl (\insns arg -> Get arg S.<| insns) (S.singleton insn) args
                              , argIDs = args
                              , extraArgsID = Nothing
                              , source = Nil
                              }
  globalDef' name $ Lambda (Right func) []

toSymbolID :: Value -> LispM Int
toSymbolID (Symbol id) = return id
toSymbolID _ = throwError $ TypeMismatch "symbol"
