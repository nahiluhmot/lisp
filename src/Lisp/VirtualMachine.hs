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
import qualified Lisp.Index as I

eval :: S.Seq Instruction -> LispM Value
eval insns =
  let evalIndex pc
        | (pc >= S.length insns) || (pc < 0) = do
          pop `catchError` \e ->
            case e of
              EmptyStack -> return $ Nil
              _ -> throwError e
        | otherwise = gcIfNecessary >> evalInstruction pc (S.index insns pc) >>= evalIndex
  in evalIndex 0

evalInstruction :: Int -> Instruction -> LispM Int
evalInstruction pc Noop = return $ succ pc
evalInstruction pc Pop = pop $> succ pc
evalInstruction pc (Push val) = push val $> succ pc
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
evalInstruction pc (MakeLambda functionID) = do
  (_, Context scope _ _) <- currentContext
  push $ Lambda functionID scope
  return $ succ pc
evalInstruction pc (Funcall argc) = do
  let defArgs ids args = foldr (uncurry IM.insert) IM.empty $ S.zip ids args
  (args S.:> fn) <- fmap S.viewr . popN $ succ argc
  case fn of
    Lambda functionID scopeIDs -> do
      func@(Function insns ids extra) <- lookupFunction functionID
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
      (ctxID, ctx) <- currentContext
      case I.insert currScope (scopes ours) of
        Nothing -> throwError FullIndex
        Just (scopeID, scopes') ->  do
          let newCtx = Context { envIDs = scopeID S.<| scopeIDs
                               , callerIDs = ctxID S.<| callerIDs ctx
                               , valStack = S.empty
                               }
          case I.insert newCtx (contexts ours) of
            Nothing -> throwError FullIndex
            Just (ctxID', ctxs') -> do
              let state = ours { context = ctxID'
                               , contexts = ctxs'
                               , scopes = scopes'
                               , currentFunc = Just func
                               }
              (result, new) <- liftIO $ runLispM (eval insns) state
              val <- either throwError return result
              put $ new { context = ctxID, currentFunc = currentFunc ours }
              push val
    _ -> throwError $ TypeMismatch "lambda"
  return $ succ pc
evalInstruction _ Return = return (-1)
evalInstruction _ (Recur argc) = do
  let defArgs ids args = mapM_ (uncurry localDef) $ S.zip ids args
  result <- gets currentFunc
  (Function insns ids extra) <- maybe (throwError RecurOutsideOfLambda) return result
  args <- popN argc
  liftIO . print $ insns
  case (S.length args `compare` S.length ids, extra) of
    (EQ, Nothing) -> defArgs ids args
    (EQ, Just id) -> defArgs (ids S.|> id) (args S.|> Nil)
    (GT, Just id) ->
      let (args', rest) = S.splitAt (S.length ids) args
      in  defArgs (ids S.|> id) (args' S.|> foldr Cons Nil rest)
    (GT, Nothing) -> throwError $ ArgMismatch (length ids) (length args)
    (LT, _) -> throwError $ ArgMismatch (length ids) (length args)
  return 0
evalInstruction pc Plus = binMath pc (+)
evalInstruction pc Minus = binMath pc (-)
evalInstruction pc Times = binMath pc (*)
evalInstruction pc Divide = binMath pc (/)
evalInstruction pc Eq = do
  [a, b] <- popN 2
  let val | a == b = symbol "t"
          | otherwise = return Nil
  val >>= push
  return $ succ pc
evalInstruction pc Neq = do
  [a, b] <- popN 2
  let val | a == b = return Nil
          | otherwise = symbol "t"
  val >>= push
  return $ succ pc
evalInstruction pc Not = do
  isNil <- fmap (== Nil) pop
  let val | isNil = symbol "t"
          | otherwise = return Nil
  val >>= push
  return $ succ pc
evalInstruction pc ICons = do
  [car, cdr] <- popN 2
  push $ Cons car cdr
  return $ succ pc
evalInstruction pc Car = do
  xs <- pop
  case xs of
    (Cons car _) -> push car
    _ -> throwError $ TypeMismatch "cons"
  return $ succ pc
evalInstruction pc Cdr = do
  xs <- pop
  case xs of
    (Cons _ cdr) -> push cdr
    _ -> throwError $ TypeMismatch "cons"
  return $ succ pc
evalInstruction pc Type = (pop >>= typeOf >>= push) $> succ pc
evalInstruction pc Print = (pop >>= printVal >> push Nil) $> succ pc

binMath :: Int -> (Rational -> Rational -> Rational) -> LispM Int
binMath pc op = do
  [a, b] <- popN 2
  case (a, b) of
    (Number x, Number y) -> push . Number $ op x y
    _ -> throwError $ TypeMismatch "number"
  return $ succ pc
