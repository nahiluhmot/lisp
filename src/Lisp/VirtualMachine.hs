{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Lisp.VirtualMachine (eval) where

import Prelude hiding (foldr, id)
import Control.Monad.State hiding (state)
import Control.Monad.Except
import Data.Foldable (foldr)
import Data.Functor
import qualified Data.Sequence as S
import qualified Data.IntMap as IM

import Lisp.Data
import Lisp.Monad

eval :: S.Seq Instruction -> LispM Value
eval insns =
  let evalIndex pc
        | (pc >= S.length insns) || (pc < 0) = do
          pop `catchError` \e ->
            case e of
              EmptyStack -> return $ Nil
              _ -> throwError e
        | otherwise = evalInstruction pc (S.index insns pc) >>= evalIndex
  in evalIndex 0

evalInstruction :: Int -> Instruction -> LispM Int
evalInstruction pc Noop = return $ succ pc
evalInstruction pc Pop = pop $> succ pc
evalInstruction pc (Push val) = push val $> succ pc
evalInstruction pc PushScope = do
  modifyContext $ \ctx@(Context scopes _) ->
    return $ (succ pc, ctx { envs = IM.empty S.<| scopes })
evalInstruction pc PopScope =
  modifyContext $ \ctx ->
    case S.viewl $ envs ctx of
      S.EmptyL -> throwError NoScope
      (_ S.:< envs') -> return (succ pc, ctx { envs = envs' })
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
evalInstruction pc (MakeLambda func) = do
  (Context es _) <- currentContext
  push $ Lambda func es
  return $ succ pc
evalInstruction pc (MakeMacro func) = do
  (Context es _) <- currentContext
  push $ Macro func es
  return $ succ pc
evalInstruction pc (Funcall argc) = do
  let defArgs ids args = foldr (uncurry IM.insert) IM.empty $ S.zip ids args
  (args S.:> fn) <- fmap S.viewr . popN $ succ argc
  case fn of
    Lambda (Left func@(NativeFunction _ _)) _ -> run func args >>= push
    Lambda (Right func@(CompiledFunction insns ids extra _)) envs' -> do
      currScope <-
        case (S.length args `compare` S.length ids, extra) of
          (EQ, Nothing) -> return $ defArgs ids args
          (EQ, Just id) -> return $ defArgs (ids S.|> id) (args S.|> Nil)
          (GT, Just id) ->
            let (args', rest) = S.splitAt (S.length ids) args
            in  return $ defArgs (ids S.|> id) (args' S.|> foldr Cons Nil rest)
          (GT, Nothing) -> throwError $ ArgMismatch (length ids) (length args)
          (LT, _) -> throwError $ ArgMismatch (length ids) (length args)
      ours <- get
      let newCtx = Context { envs = currScope S.<| envs'
                           , valStack = S.empty
                           }
      let state = ours { context = newCtx, currentFunc = Just func }
      (result, new) <- liftIO $ runLispM (eval insns) state
      val <- either throwError return result
      put $ new { context = context ours, currentFunc = currentFunc ours }
      push val
    _ -> throwError $ TypeMismatch "lambda"
  return $ succ pc
evalInstruction _ Return = return (-1)
evalInstruction _ (Recur argc) = do
  let defArgs ids args = localDef' $ S.zip ids args
  result <- gets currentFunc
  (CompiledFunction _ ids extra _) <- maybe (throwError RecurOutsideOfLambda) return result
  args <- popN argc
  case (S.length args `compare` S.length ids, extra) of
    (EQ, Nothing) -> defArgs ids args
    (EQ, Just id) -> defArgs (ids S.|> id) (args S.|> Nil)
    (GT, Just id) ->
      let (args', rest) = S.splitAt (S.length ids) args
      in  defArgs (ids S.|> id) (args' S.|> foldr Cons Nil rest)
    (GT, Nothing) -> throwError $ ArgMismatch (length ids) (length args)
    (LT, _) -> throwError $ ArgMismatch (length ids) (length args)
  return 0
