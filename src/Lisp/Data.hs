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

type NativeMacro = (Int, Seq Value -> LispM (Seq Instruction))

type NativeFunction = (Int, Seq Value -> LispM Value)

data CompiledFunction = CompiledFunction { instructions :: Seq Instruction
                                         , argIDs       :: Seq Int
                                         , extraArgsID  :: Maybe Int
                                         , source       :: Value
                                         }

data Instruction = Noop
                 | Pop
                 | Push Value
                 | PushScope
                 | PopScope
                 | Def Int
                 | Get Int
                 | Set Int
                 | Jump Int
                 | BranchIf Int
                 | BranchUnless Int
                 | MakeLambda CompiledFunction
                 | MakeMacro CompiledFunction
                 | Funcall Int
                 | Return
                 | Recur Int
                 | PushErrorHandler
                 | PopErrorHandler

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

macro :: Int -> (Seq Value -> LispM (Seq Instruction)) -> Value
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
