{-# LANGUAGE OverloadedLists #-}

module Lisp.Data where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Sequence as S
import Data.Text
import Data.IntMap.Strict as IM

import Lisp.SymbolTable as ST

data Value = Nil
           | Number Rational
           | Symbol Int
           | String Text
           | Quote Value
           | List Value (Seq Value)
           | DottedList Value (Seq Value) Value
           | Lambda Function
           | Macro Macro
           | Error LispError

type Function = Either NativeFunction (CompiledFunction, [Env])

type Macro = Either NativeMacro (CompiledFunction, [Env])

type NativeMacro = (Int, Seq Value -> LispM Instruction)

type NativeFunction = (Int, Seq Value -> LispM Value)

data CompiledFunction = CompiledFunction { instructions :: Instruction
                                         , argIDs       :: Seq Int
                                         , extraArgsID  :: Maybe Int
                                         , source       :: Value
                                         }

data Instruction = Halt
                 | Return
                 | Recur Int
                 | Push Value Instruction
                 | PushScope Instruction
                 | PopScope Instruction
                 | PushErrorHandler Instruction
                 | PopErrorHandler Instruction
                 | Def Int Instruction
                 | Get Int Instruction
                 | Set Int Instruction
                 | If Instruction Instruction
                 | MakeLambda CompiledFunction Instruction
                 | MakeMacro CompiledFunction Instruction
                 | Funcall Int Instruction

type Env = IntMap Value

data LispState = LispState { symbolTable   :: SymbolTable Text
                           , globals       :: Env
                           , scope         :: [Env]
                           , stack         :: [Value]
                           , funcs         :: [CompiledFunction]
                           , errorHandlers :: [Function]
                           }

data LispError = LispError { errType :: Int, errMessage :: Text }
                 deriving (Eq, Show)

type LispM = ExceptT LispError (StateT LispState IO)

emptyLispState :: LispState
emptyLispState =
  LispState { symbolTable = ST.empty
            , globals = IM.empty
            , scope = []
            , stack = []
            , funcs = []
            , errorHandlers = []
            }

list :: Seq Value -> Value
list xs =
  case viewl xs of
    EmptyL -> Nil
    (x :< xs') -> List x xs'

dottedList :: Seq Value -> Value -> Value
dottedList xs y =
  case viewl xs of
    EmptyL -> y
    (x :< xs') ->
      case y of
        Nil -> List x xs'
        _ -> DottedList x xs' y

toSeq :: Value -> Either (Seq Value, Value) (Seq Value)
toSeq =
  let go (List x xs) = Right (x <| xs)
      go (DottedList x xs y) = Left (x <| xs, y)
      go Nil = Right []
      go val = Left ([], val)
  in  go

macro :: Int -> (Seq Value -> LispM Instruction) -> Value
macro name func = Macro (Left (name, func))

function :: Int -> (Seq Value -> LispM Value) -> Value
function name func = Lambda (Left (name, func))

instance Eq Value where
  (==) Nil Nil = True
  (==) (Number x) (Number y) = x == y
  (==) (Symbol x) (Symbol y) = x == y
  (==) (String x) (String y) = x == y
  (==) (Quote x) (Quote y) = x == y
  (==) (List x xs) (List y ys) = (x == y) && (xs == ys)
  (==) (DottedList x xs x') (DottedList y ys y') =
    (xs == ys) && (x == y) && (x' == y')
  (==) (Error x) (Error y) = x == y
  (==) _ _ = False

instance Monoid Instruction where
  mempty = Halt
  mappend m n =
    let go Halt = n
        go Return = Return
        go (Recur i) = Recur i
        go (Push v m') = Push v (go m')
        go (PushScope m') = PushScope (go m')
        go (PopScope m') = PopScope (go m')
        go (PushErrorHandler m') = PushErrorHandler (go m')
        go (PopErrorHandler m') = PopErrorHandler (go m')
        go (Def i m') = Def i (go m')
        go (Get i m') = Get i (go m')
        go (Set i m') = Set i (go m')
        go (If m' m'') = If (go m') (go m'')
        go (MakeLambda c m') = MakeLambda c (go m')
        go (MakeMacro c m') = MakeMacro c (go m')
        go (Funcall i m') = Funcall i (go m')
    in  go m
