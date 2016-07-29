{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Lisp.Builtins where

import Prelude hiding (id)
import Control.Monad.Except
import Control.Monad.State hiding (state)
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Text as T

import Lisp.Data
import Lisp.Monad
import Lisp.VirtualMachine
import qualified Lisp.Index as I

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
  defFunctionLikeInstruction "+" Plus 2
  defFunctionLikeInstruction "-" Minus 2
  defFunctionLikeInstruction "*" Times 2
  defFunctionLikeInstruction "/" Divide 2
  defFunctionLikeInstruction "eq" Eq 2
  defFunctionLikeInstruction "neq" Neq 2
  defFunctionLikeInstruction "not" Not 1
  defFunctionLikeInstruction "cons" ICons 2
  defFunctionLikeInstruction "car" Car 1
  defFunctionLikeInstruction "cdr" Cdr 1
  defFunctionLikeInstruction "type-of" Type 1
  defFunctionLikeInstruction "puts" Print 1
  defFunctionLikeInstruction "read" Read 1
  defFunctionLikeInstruction "eval" Eval 1
  defFunctionLikeInstruction "gets" GetLine 0

compileLambda :: S.Seq Value -> LispM (S.Seq Instruction)
compileLambda vals = symbol "lambda" >>= \id -> compileFunc MakeLambda id vals

compileMacro :: S.Seq Value -> LispM (S.Seq Instruction)
compileMacro vals = symbol "macro" >>= \id -> compileFunc MakeMacro id vals

compileFunc :: (Int -> Instruction) -> Value -> S.Seq Value -> LispM (S.Seq Instruction)
compileFunc toInsn name list =
  let namesToIDs = F.foldlM (\acc x -> (acc S.|>) <$> toSymbolID x) S.empty
  in  case S.viewl list of
        S.EmptyL -> throwError $ ArgMismatch 1 0
        (args S.:< body) -> do
          (ids, extra) <-
            case toSeq args of
              (Left (xs, x)) -> (,) <$> namesToIDs xs <*> (Just <$> toSymbolID x)
              (Right xs) -> (, Nothing) <$> namesToIDs xs
          insns <- compileValues body
          let function = Function { instructions = insns
                                  , argIDs = ids
                                  , extraArgsID = extra
                                  , source = Right . Cons name $ foldr Cons Nil list
                                  }
          result <- gets (I.insert function . functions)
          case result of
            Nothing -> throwError FullIndex
            Just (newID, fs') -> do
              modify $ \state -> state { functions = fs' }
              return [toInsn newID]

compileLet :: S.Seq Value -> LispM (S.Seq Instruction)
compileLet list = do
  when (S.null list) $ throwError $ ArgMismatch 2 0
  let (defs S.:< body) = S.viewl list
      go sexp =
        case toSeq sexp of
          Right [Symbol id, value] -> (S.|> Set id) <$> compile value
          _ -> throwError $ InvalidLet sexp
  case toSeq defs of
    Right conses -> do
      bodyInsns <- compileValues body
      (PushScope S.<|) <$> F.foldrM (\def acc -> (S.>< acc) <$> go def) (bodyInsns S.|> PopScope) conses
    Left _ -> throwError $ InvalidLet defs

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
      func = Function { instructions = F.foldl (\insns arg -> Get arg S.<| insns) (S.singleton insn) args
                      , argIDs = args
                      , extraArgsID = Nothing
                      , source = Left name
                      }
  result <- gets $ I.insert func . functions
  case result of
    Nothing -> throwError FullIndex
    (Just (funcID, functions')) -> do
      modify $ \state -> state { functions = functions' }
      globalDef' name $ Lambda funcID []

toSymbolID :: Value -> LispM Int
toSymbolID (Symbol id) = return id
toSymbolID _ = throwError $ TypeMismatch "symbol"
