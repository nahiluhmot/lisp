module Lisp.Data where

import Control.Monad.Except
import Control.Monad.State
import Data.Sequence as S
import Data.Text
import Data.IntMap as IM
import Text.Parsec (ParseError)

import Lisp.SymbolTable as ST
import Lisp.Index as I

data Value = Nil
           | Number Rational
           | Symbol Int
           | String Text
           | Quote Value
           | Cons Value Value
           | Lambda Function (Seq Int)
           | Macro Function (Seq Int)

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
                 | Plus
                 | Minus
                 | Times
                 | Divide
                 | Eq
                 | Neq
                 | Not
                 | ICons
                 | Car
                 | Cdr
                 | Type
                 | Print
                 | GetLine
                 | Read
                 | Eval

type Function = Either NativeFunction CompiledFunction

data NativeFunction = NativeFunction { name :: Text
                                     , run  :: S.Seq Value -> LispM Value
                                     }

data CompiledFunction = CompiledFunction { instructions :: Seq Instruction
                                         , argIDs       :: Seq Int
                                         , extraArgsID  :: Maybe Int
                                         , source       :: Value
                                         }

data Context = Context { envIDs    :: Seq Int
                       , callerIDs :: Seq Int
                       , valStack  :: Seq Value
                       }

data LispState = LispState { symbolTable :: SymbolTable Text
                           , globals     :: Env
                           , scopes      :: Index Env
                           , contexts    :: Index Context
                           , builtins    :: IntMap (S.Seq Value -> LispM (S.Seq Instruction))
                           , context     :: Int
                           , currentFunc :: Maybe CompiledFunction
                           , nextGC      :: Integer
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
emptyContext = Context { envIDs = S.empty
                       , callerIDs = S.empty
                       , valStack = S.empty
                       }

emptyLispState :: LispState
emptyLispState =
  case I.insert emptyContext I.empty of
    Nothing -> error "Unable to create empty context"
    Just (ctxID, ctxs) ->
      LispState { symbolTable = ST.empty
                , globals = IM.empty
                , builtins = IM.empty
                , scopes = I.empty
                , contexts = ctxs
                , context = ctxID
                , currentFunc = Nothing
                , nextGC = 1024
                }

toSeq :: Value -> Either (Seq Value, Value) (Seq Value)
toSeq =
  let go acc (Cons car cdr) = go (acc |> car) cdr
      go acc Nil = Right acc
      go acc val = Left (acc, val)
  in  go S.empty
