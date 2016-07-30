{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Lisp.VirtualMachine (compile, compileValues, eval) where

import Prelude hiding (foldr, id)
import Control.Monad.State hiding (state)
import Control.Monad.Except
import Data.Foldable (foldr, foldrM)
import Data.Functor
import qualified Data.Sequence as S
import qualified Data.IntMap as IM
import qualified Data.Text.IO as IO
import qualified Data.Traversable as T

import Lisp.Data
import Lisp.Monad
import Lisp.Parser (parse)
import qualified Lisp.Index as I

compile :: Value -> LispM (S.Seq Instruction)
compile ast =
  case toSeq ast of
    Left (list, literal) -> do
      unless (S.null list) $ throwError CompileDottedList
      case literal of
        Symbol sym -> return $ S.singleton (Get sym)
        Quote lit -> return $ S.singleton (Push lit)
        lit -> return $ S.singleton (Push lit)
    Right list ->
      case S.viewl list of
        S.EmptyL -> return $ S.singleton (Push Nil)
        (Symbol fn S.:< args) -> do
          result <- lookupBuiltin fn
          case result of
            Just func -> func args
            Nothing -> do
              result' <- fmap Just (lookupSymbol fn) `catchError` const (return Nothing)
              case result' of
                Just (Macro fID scopeIDs) -> do
                  let insns = Push (Lambda fID scopeIDs) S.<| (fmap Push (S.reverse args) S.|> Funcall (S.length args))
                  expanded <- eval insns
                  compile expanded
                _ -> (S.|> Funcall (S.length args)) <$> ((Get fn S.<|) <$> compileValues args)
        (fn@(Cons _ _) S.:< args) ->
          (S.|> Funcall (S.length args)) <$> ((S.><) <$> compile fn <*> compileValues args)
        _ -> throwError $ TypeMismatch "symbol"

compileValues :: S.Seq Value -> LispM (S.Seq Instruction)
compileValues = foldrM (\ast acc -> (acc S.><) <$> compile ast) S.empty

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
evalInstruction pc PushScope = do
  result <- gets $ I.insert IM.empty . scopes
  (scopeID, scopes') <- maybe (throwError FullIndex) return result
  modify $ \state -> state { scopes = scopes' }
  modifyContext $ \ctx -> return $ (succ pc, ctx { envIDs = scopeID S.<| envIDs ctx })
evalInstruction pc PopScope =
  modifyContext $ \ctx ->
    case S.viewl $ envIDs ctx of
      S.EmptyL -> throwError NoScope
      (_ S.:< envIDs') -> return (succ pc, ctx { envIDs = envIDs' })
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
  (_, Context scope _ _) <- currentContext
  push $ Lambda func scope
  return $ succ pc
evalInstruction pc (MakeMacro functionID) = do
  (_, Context scope _ _) <- currentContext
  push $ Macro functionID scope
  return $ succ pc
evalInstruction pc (Funcall argc) = do
  let defArgs ids args = foldr (uncurry IM.insert) IM.empty $ S.zip ids args
  (args S.:> fn) <- fmap S.viewr . popN $ succ argc
  case fn of
    Lambda func@(Function insns ids extra _) scopeIDs -> do
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
  let defArgs ids args = localDef' $ S.zip ids args
  result <- gets currentFunc
  (Function _ ids extra _) <- maybe (throwError RecurOutsideOfLambda) return result
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
evalInstruction pc GetLine = (liftIO IO.getLine >>= push . String) $> succ pc
evalInstruction pc Eval = do
  uncompiled <- pop
  case toSeq uncompiled of
    Left _ -> throwError $ CompileDottedList
    Right vals -> do
      result <- S.viewr <$> T.mapM (compile >=> eval) vals
      case result of
        S.EmptyR -> push Nil
        (_ S.:> x) -> push x
  return $ succ pc
evalInstruction pc Read = do
  val <- pop
  case val of
    String text -> parse text >>= push . foldr Cons Nil
    _ -> throwError $ TypeMismatch "string"
  return $ succ pc

binMath :: Int -> (Rational -> Rational -> Rational) -> LispM Int
binMath pc op = do
  [a, b] <- popN 2
  case (a, b) of
    (Number x, Number y) -> push . Number $ op x y
    _ -> throwError $ TypeMismatch "number"
  return $ succ pc
