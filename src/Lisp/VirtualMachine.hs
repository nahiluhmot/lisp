{-# LANGUAGE OverloadedStrings #-}

module Lisp.VirtualMachine (eval, funcall) where

import Prelude as P
import Control.Monad.Except
import Control.Monad.State.Strict hiding (state)
import Data.Functor
import Data.Sequence as S
import qualified Data.IntMap.Strict as IM

import Lisp.Data
import Lisp.Core

eval :: Seq Instruction -> LispM Value
eval insns =
  let evalIndex pc
        | (pc >= S.length insns) || (pc < 0) = return ()
        | otherwise = evalInstruction pc (index insns pc) >>= evalIndex
  in   do
    old <- get
    put $ old { stack = S.empty }
    catchError (evalIndex 0) handleError
    new <- get
    let result | S.null (stack new) = Nil
               | otherwise = S.index (stack new) 0
    put $ new { stack = stack old }
    return result

evalInstruction :: Int -> Instruction -> LispM Int
evalInstruction pc Noop = return $ succ pc
evalInstruction pc Pop = pop $> succ pc
evalInstruction pc (Push val) = push val $> succ pc
evalInstruction pc PushScope =
  modifyScope $ \scopes ->
    return $ (succ pc, IM.empty : scopes)
evalInstruction pc PopScope =
  modifyScope $ \scopes ->
    case scopes of
      [] -> raiseNoScope
      (_ : scope') -> return (succ pc, scope')
evalInstruction pc (Def sym) = (pop >>= def sym >> push (Symbol sym)) $> succ pc
evalInstruction pc (Get sym) = (lookupSymbol sym >>= push) $> succ pc
evalInstruction pc (Set sym) = (pop >>= localDef sym >> push (Symbol sym)) $> succ pc
evalInstruction pc (Jump idx) = return $ pc + idx
evalInstruction pc (BranchIf idx) =
  let branchIf Nil = succ pc
      branchIf _ = pc + idx
  in  branchIf <$> pop
evalInstruction pc (BranchUnless idx) =
  let branchUnless Nil = pc + idx
      branchUnless _ = succ pc
  in  branchUnless <$> pop
evalInstruction pc (MakeLambda func) = succ pc <$ do
  envs <- gets scope
  push $ Lambda (Right (func, envs))
evalInstruction pc (MakeMacro func) = succ pc <$ do
  envs <- gets scope
  push $ Macro (Right (func, envs))
evalInstruction pc (Funcall argc) = succ pc <$ do
  (fn :< args) <- fmap viewl . popN $ succ argc
  case fn of
    (Lambda func) -> funcall func args >>= push
    _ -> raiseTypeMismatch "lambda" fn
evalInstruction _ Return = return (-1)
evalInstruction _ (Recur argc) = 0 <$ do
  result <- gets currentFunc
  (CompiledFunction _ ids extra _) <- maybe raiseInvalidRecur return result
  popN argc >>= matchArgs ids extra >>= localDef'
evalInstruction _ Raise = do
  tup <- pop2
  case tup of
    (Symbol sym, String msg) -> raise' sym msg
    (Symbol _, given) -> raiseTypeMismatch "string" given
    (given, _) -> raiseTypeMismatch "symbol" given
evalInstruction pc PushErrorHandler = succ pc <$ do
  val <- pop
  case val of
    (Lambda func) ->
      modify $ \state -> state { errorHandlers = func : errorHandlers state }
    _ -> raiseTypeMismatch "lambda" val
evalInstruction pc PopErrorHandler = succ pc <$ do
  state <- get
  when (P.null $ errorHandlers state) raiseNoErrorHandlers
  put $ state { errorHandlers = tail $ errorHandlers state }

funcall :: Function -> Seq Value -> LispM Value
funcall (Left (_, run)) args = run args
funcall (Right (func@(CompiledFunction insns ids extra _), scope')) args = do
  currScope <- matchArgs ids extra args
  ours <- get
  put $ ours { scope = currScope : scope', currentFunc = Just func }
  val <- eval insns
  modify $ \curr -> curr { scope = scope ours, currentFunc = currentFunc ours }
  return val

handleError :: LispError -> LispM ()
handleError err =
  let go [] = throwError err
      go (handler : handlers) = do
        modify $ \state -> state { errorHandlers = handlers }
        funcall handler . singleton $ Error err
  in  gets errorHandlers >>= go >>= push
