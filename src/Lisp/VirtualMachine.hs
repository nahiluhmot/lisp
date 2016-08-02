{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Lisp.VirtualMachine (eval) where

import Prelude hiding (foldr, id, length, splitAt, zip)
import Control.Monad.State hiding (state)
import Control.Monad.Except
import Data.Foldable (foldr)
import Data.Functor
import Data.Sequence
import Data.IntMap hiding (foldr)

import Lisp.Data
import Lisp.Monad

eval :: Seq Instruction -> LispM Value
eval insns =
  let evalIndex pc
        | (pc >= length insns) || (pc < 0) = do
          result <- pop `catchError` \e ->
            case e of
              EmptyStack -> return $ Nil
              _ -> throwError e
          modify $ \state -> state { stack = [] }
          return result
        | otherwise = evalInstruction pc (index insns pc) >>= evalIndex
  in modify (\state -> state { stack = [] }) >> evalIndex 0

evalInstruction :: Int -> Instruction -> LispM Int
evalInstruction pc Noop = return $ succ pc
evalInstruction pc Pop = pop $> succ pc
evalInstruction pc (Push val) = push val $> succ pc
evalInstruction pc PushScope =
  modifyScope $ \scopes ->
    return $ (succ pc, [] <| scopes)
evalInstruction pc PopScope =
  modifyScope $ \scopes ->
    case viewl scopes of
      EmptyL -> throwError NoScope
      (_ :< scope') -> return (succ pc, scope')
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
evalInstruction pc (MakeMacro macro) = (gets scope >>= push . Macro (Right macro)) $> succ pc
evalInstruction pc (Funcall argc) = do
  (args :> fn) <- fmap viewr . popN $ succ argc
  case fn of
    Lambda (Left (_, run)) _ -> run args >>= push
    Lambda (Right func@(CompiledFunction insns ids extra _)) scope' -> do
      currScope <- foldr (uncurry insert) [] <$> matchArgs ids extra args
      ours <- get
      put $ ours { scope = currScope <| scope', currentFunc = Just func }
      val <- eval insns
      modify $ \state->
        state { scope = scope ours
              , stack = val <| stack ours
              , currentFunc = currentFunc ours
              }
    _ -> throwError $ TypeMismatch "lambda"
  return $ succ pc
evalInstruction _ Return = return (-1)
evalInstruction _ (Recur argc) = do
  result <- gets currentFunc
  (CompiledFunction _ ids extra _) <- maybe (throwError RecurOutsideOfLambda) return result
  args <- popN argc
  matched <- matchArgs ids extra args
  localDef' matched
  return 0
