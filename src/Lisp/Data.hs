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
           | Macro Function (Seq Env)

type Env = IntMap Value

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
                 | MakeLambda Function
                 | MakeMacro Function
                 | Funcall Int
                 | Return
                 | Recur Int

type Function = Either NativeFunction CompiledFunction

data NativeFunction = NativeFunction { name :: Text
                                     , run  :: Seq Value -> LispM Value
                                     }

data CompiledFunction = CompiledFunction { instructions :: Seq Instruction
                                         , argIDs       :: Seq Int
                                         , extraArgsID  :: Maybe Int
                                         , source       :: Value
                                         }

data Context = Context { envs      :: Seq Env
                       , valStack  :: Seq Value
                       }

data LispState = LispState { symbolTable :: SymbolTable Text
                           , globals     :: Env
                           , builtins    :: IntMap (Seq Value -> LispM (Seq Instruction))
                           , context     :: Context
                           , currentFunc :: Maybe CompiledFunction
                           }

data LispError = TypeMismatch Text
               | EmptyStack
               | NoScope
               | NoSuchScope Int
               | NoContext
               | NoSuchContext Int
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

emptyContext :: Context
emptyContext = Context { envs = S.empty
                       , valStack = S.empty
                       }

emptyLispState :: LispState
emptyLispState =
  LispState { symbolTable = ST.empty
            , globals = IM.empty
            , builtins = IM.empty
            , context = emptyContext
            , currentFunc = Nothing
            }

toSeq :: Value -> Either (Seq Value, Value) (Seq Value)
toSeq =
  let go acc (Cons car cdr) = go (acc |> car) cdr
      go acc Nil = Right acc
      go acc val = Left (acc, val)
  in  go S.empty
