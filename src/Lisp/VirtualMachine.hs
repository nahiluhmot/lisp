{-# LANGUAGE OverloadedStrings #-}

module Lisp.VirtualMachine (eval, funcall, funcallByName) where

import Prelude as P
import Control.Monad.Except
import Control.Monad.State.Strict hiding (state)
import Data.Maybe (fromMaybe)
import Data.Sequence as S
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM

import Lisp.Data
import Lisp.Core

eval :: Instruction -> LispM Value
eval insn = do
  old <- gets stack
  val <- evalInstruction insn `catchError` (\e -> handleError e >> pop)
  modify $ \new -> new { stack = old }
  pure val

evalInstruction :: Instruction -> LispM Value
evalInstruction Halt = fromMaybe Nil <$> safePop
evalInstruction Return = pop
evalInstruction (Recur argc) = do
  args <- popN argc
  fs <- gets funcs
  when (P.null fs) raiseInvalidRecur
  let (CompiledFunction insn ids extra _) = head fs
  imap <- matchArgs ids extra args
  localDef' imap
  evalInstruction insn
evalInstruction (Push v i) = push v >> evalInstruction i
evalInstruction (PushScope i) = do
  modifyScope $ \scopes ->
    pure ((), IM.empty : scopes)
  evalInstruction i
evalInstruction (PopScope i) =
  let go [] = raiseNoScope
      go (_ : scope') = pure ((), scope')
  in  modifyScope go >> evalInstruction i
evalInstruction (PushErrorHandler i) = do
  val <- pop
  case val of
    (Lambda func) ->
      modify $ \state -> state { errorHandlers = func : errorHandlers state }
    _ -> raiseTypeMismatch "lambda" val
  evalInstruction i
evalInstruction (PopErrorHandler i) = do
  state <- get
  when (P.null $ errorHandlers state) raiseNoErrorHandlers
  put $ state { errorHandlers = tail $ errorHandlers state }
  evalInstruction i
evalInstruction (Def sym i) = pop >>= def sym >> push (Symbol sym) >> evalInstruction i
evalInstruction (Get sym i) = lookupSymbol sym >>= push >> evalInstruction i
evalInstruction (Set sym i) = pop >>= localDef sym >> push (Symbol sym) >> evalInstruction i
evalInstruction (If t e) =
  let branchIf Nil = e
      branchIf _ = t
  in  pop >>= evalInstruction . branchIf
evalInstruction (MakeLambda func i) =
  gets scope >>= \envs -> push (Lambda (Right (func, envs))) >> evalInstruction i
evalInstruction (MakeMacro func i) =
  gets scope >>= \envs -> push (Macro (Right (func, envs))) >> evalInstruction i
evalInstruction (Funcall argc i) = do
  (fn :< args) <- viewl <$> popN (succ argc)
  case fn of
    (Lambda func) -> funcall func args >>= push
    _ -> raiseTypeMismatch "lambda" fn
  evalInstruction i

funcall :: Function -> Seq Value -> LispM Value
funcall (Left (_, run)) args = run args
funcall (Right (func@(CompiledFunction insns ids extra _), scope')) args = do
  currScope <- matchArgs ids extra args
  ours <- get
  put $ ours { scope = currScope : scope', funcs = func : funcs ours }
  val <- eval insns
  modify $ \curr -> curr { scope = scope ours, funcs = funcs ours }
  return val

funcallByName :: Text -> Seq Value -> LispM Value
funcallByName name args = do
  symbolID <- symToID name
  value <- lookupSymbol symbolID
  case value of
    Lambda func -> funcall func args
    _ -> raiseTypeMismatch "function" value

handleError :: LispError -> LispM ()
handleError err =
  let go [] = throwError err
      go (handler : handlers) = do
        modify $ \state -> state { errorHandlers = handlers }
        funcall handler . singleton $ Error err
  in  gets errorHandlers >>= go >>= push
