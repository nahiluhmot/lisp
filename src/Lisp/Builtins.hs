{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Lisp.Builtins where

import Prelude hiding (id)
import Control.Monad.Except
import Control.Monad.State hiding (state)
import qualified Data.Foldable as F
import qualified Data.Sequence as S

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
  compileFunctionLikeInstruction Plus 2 >>= globalDef' "+"
  compileFunctionLikeInstruction Minus 2 >>= globalDef' "-"
  compileFunctionLikeInstruction Times 2 >>= globalDef' "*"
  compileFunctionLikeInstruction Divide 2 >>= globalDef' "/"
  compileFunctionLikeInstruction Eq 2 >>= globalDef' "eq"
  compileFunctionLikeInstruction Neq 2 >>= globalDef' "neq"
  compileFunctionLikeInstruction Not 1 >>= globalDef' "not"
  compileFunctionLikeInstruction ICons 2 >>= globalDef' "cons"
  compileFunctionLikeInstruction Car 1 >>= globalDef' "car"
  compileFunctionLikeInstruction Cdr 1 >>= globalDef' "cdr"
  compileFunctionLikeInstruction Type 1 >>= globalDef' "type-of"
  compileFunctionLikeInstruction Print 1 >>= globalDef' "print"
  compileFunctionLikeInstruction Read 1 >>= globalDef' "read"
  compileFunctionLikeInstruction Eval 1 >>= globalDef' "eval"

compileLambda :: S.Seq Value -> LispM (S.Seq Instruction)
compileLambda = compileFunc MakeLambda

compileMacro :: S.Seq Value -> LispM (S.Seq Instruction)
compileMacro = compileFunc MakeMacro

compileFunc :: (Int -> Instruction) -> S.Seq Value -> LispM (S.Seq Instruction)
compileFunc toInsn list =
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

compileFunctionLikeInstruction :: Instruction -> Int -> LispM Value
compileFunctionLikeInstruction insn argc = do
  let args = [0 .. pred argc]
      func = Function { instructions = F.foldl (\insns arg -> Get arg S.<| insns) (S.singleton insn) args
                      , argIDs = args
                      , extraArgsID = Nothing
                      }
  result <- gets $ I.insert func . functions
  case result of
    Nothing -> throwError FullIndex
    (Just (funcID, functions')) -> do
      modify $ \state -> state { functions = functions' }
      return $ Lambda funcID []

toSymbolID :: Value -> LispM Int
toSymbolID (Symbol id) = return id
toSymbolID _ = throwError $ TypeMismatch "symbol"
