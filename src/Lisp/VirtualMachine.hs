{-# LANGUAGE OverloadedStrings #-}

module Lisp.VirtualMachine (eval, funcall) where

import Prelude as P
import Control.Monad.State.Strict hiding (state)
import Data.Functor
import Data.Sequence as S
import qualified Data.IntMap.Strict as IM

import Lisp.Data
import Lisp.Monad

eval :: Seq Instruction -> LispM Value
eval insns =
  let evalIndex pc
        | (pc >= S.length insns) || (pc < 0) = do
          state <- get
          return $
            case viewl $ stack state of
              EmptyL -> Nil
              (first :< _) -> first
        | otherwise = evalInstruction pc (index insns pc) >>= evalIndex
  in  do
    old <- get
    put $ old { stack = S.empty }
    result <- evalIndex 0
    modify $ \state -> state { stack = stack old }
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
evalInstruction pc (Def sym) = (pop >>= globalDef sym) $> succ pc
evalInstruction pc (Get sym) = (lookupSymbol sym >>= push) $> succ pc
evalInstruction pc (Set sym) = (pop >>= localDef sym) $> succ pc
evalInstruction pc (Jump idx) = return $ pc + idx
evalInstruction pc (BranchIf idx) =
  let branchIf Nil = succ pc
      branchIf _ = pc + idx
  in  branchIf <$> pop
evalInstruction pc (BranchUnless idx) =
  let branchUnless Nil = pc + idx
      branchUnless _ = succ pc
  in  branchUnless <$> pop
evalInstruction pc (MakeLambda func) = (gets scope >>= push . Lambda (Right func)) $> succ pc
evalInstruction pc (MakeMacro mac) = (gets scope >>= push . Macro (Right mac)) $> succ pc
evalInstruction pc (Funcall argc) = succ pc <$ do
  (fn :< args) <- fmap viewl . popN $ succ argc
  funcall fn args >>= push
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
evalInstruction pc (PushErrorHandler func) =
  succ pc <$ modify (\state -> state { errorHandlers = func : errorHandlers state })
evalInstruction pc PopErrorHandler = succ pc <$ do
  state <- get
  when (P.null $ errorHandlers state) raiseNoErrorHandlers
  put $ state { errorHandlers = tail $ errorHandlers state }

funcall :: Value -> Seq Value -> LispM Value
funcall (Lambda (Left (_, run)) _) args = run args
funcall (Lambda (Right func@(CompiledFunction insns ids extra _)) scope') args = do
  currScope <- matchArgs ids extra args
  ours <- get
  put $ ours { scope = currScope : scope', currentFunc = Just func }
  val <- eval insns
  modify $ \curr -> curr { scope = scope ours, currentFunc = currentFunc ours }
  return val
funcall val _ = raiseTypeMismatch "lambda" val
