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
import qualified Data.IntMap as M

import Lisp.Data
import Lisp.Monad

eval :: Seq Instruction -> LispM Value
eval insns =
  let evalIndex pc
        | (pc >= length insns) || (pc < 0) = do
          pop `catchError` \e ->
            case e of
              EmptyStack -> return $ Nil
              _ -> throwError e
        | otherwise = evalInstruction pc (index insns pc) >>= evalIndex
  in evalIndex 0

evalInstruction :: Int -> Instruction -> LispM Int
evalInstruction pc Noop = return $ succ pc
evalInstruction pc Pop = pop $> succ pc
evalInstruction pc (Push val) = push val $> succ pc
evalInstruction pc PushScope = do
  modifyScope $ \scopes ->
    return $ (succ pc, M.empty <| scopes)
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
  let defArgs ids args = foldr (uncurry M.insert) M.empty $ zip ids args
  (args :> fn) <- fmap viewr . popN $ succ argc
  case fn of
    Lambda (Left (_, run)) _ -> run args >>= push
    Lambda (Right func@(CompiledFunction insns ids extra _)) scope' -> do
      currScope <-
        case (length args `compare` length ids, extra) of
          (EQ, Nothing) -> return $ defArgs ids args
          (EQ, Just id) -> return $ defArgs (ids |> id) (args |> Nil)
          (GT, Just id) ->
            let (args', rest) = splitAt (length ids) args
            in  return $ defArgs (ids |> id) (args' |> foldr Cons Nil rest)
          (GT, Nothing) -> throwError $ ArgMismatch (length ids) (length args)
          (LT, _) -> throwError $ ArgMismatch (length ids) (length args)
      ours <- get
      put $ ours { scope = currScope <| scope', stack = empty, currentFunc = Just func }
      val <- eval insns
      modify $ \state ->
        state { scope = scope ours
              , stack = val <| stack ours
              , currentFunc = currentFunc ours
              }
    _ -> throwError $ TypeMismatch "lambda"
  return $ succ pc
evalInstruction _ Return = return (-1)
evalInstruction _ (Recur argc) = do
  let defArgs ids args = localDef' $ zip ids args
  result <- gets currentFunc
  (CompiledFunction _ ids extra _) <- maybe (throwError RecurOutsideOfLambda) return result
  args <- popN argc
  case (length args `compare` length ids, extra) of
    (EQ, Nothing) -> defArgs ids args
    (EQ, Just id) -> defArgs (ids |> id) (args |> Nil)
    (GT, Just id) ->
      let (args', rest) = splitAt (length ids) args
      in  defArgs (ids |> id) (args' |> foldr Cons Nil rest)
    (GT, Nothing) -> throwError $ ArgMismatch (length ids) (length args)
    (LT, _) -> throwError $ ArgMismatch (length ids) (length args)
  return 0
