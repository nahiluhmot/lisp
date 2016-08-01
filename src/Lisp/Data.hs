module Lisp.Data where

import Control.Monad.Except
import Control.Monad.State
import Data.Sequence as S
import Data.Text
import Data.IntMap as IM
import Text.Parsec (ParseError)

import Lisp.SymbolTable as ST

data Value = Nil
           | Number Rational
           | Symbol Int
           | String Text
           | Quote Value
           | Cons Value Value
           | Lambda Function (Seq Env)
           | Macro Macro (Seq Env)

type Function = Either NativeFunction CompiledFunction

type Macro = Either NativeMacro CompiledFunction

type NativeMacro = (Text, Seq Value -> LispM (Seq Instruction))

type NativeFunction = (Text, Seq Value -> LispM Value)

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

type Env = IntMap Value

data LispState = LispState { symbolTable :: SymbolTable Text
                           , globals     :: Env
                           , scope       :: Seq Env
                           , stack       :: Seq Value
                           , currentFunc :: Maybe CompiledFunction
                           }

data LispError = TypeMismatch Text
               | EmptyStack
               | NoScope
               | UndefinedValue Text
               | UnsetSymbol Int
               | ArgMismatch Int Int
               | NoSuchFunction Int
               | CompileDottedList
               | NotImplemented Text
               | ParseError ParseError
               | FullIndex
               | RecurOutsideOfLambda
               | InvalidLet Text
               deriving (Show)

type LispM = ExceptT LispError (StateT LispState IO)

emptyLispState :: LispState
emptyLispState =
  LispState { symbolTable = ST.empty
            , globals = IM.empty
            , scope = S.empty
            , stack = S.empty
            , currentFunc = Nothing
            }

toSeq :: Value -> Either (Seq Value, Value) (Seq Value)
toSeq =
  let go acc (Cons car cdr) = go (acc |> car) cdr
      go acc Nil = Right acc
      go acc val = Left (acc, val)
  in  go S.empty

instance Eq Value where
  (==) Nil Nil = True
  (==) (Number x) (Number y) = x == y
  (==) (Symbol x) (Symbol y) = x == y
  (==) (String x) (String y) = x == y
  (==) (Quote x) (Quote y) = x == y
  (==) (Cons x y) (Cons x' y') = (x == x') && (y == y')
  (==) _ _ = False
