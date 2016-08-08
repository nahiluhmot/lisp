{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Lisp.Monad where

import Prelude hiding (id)
import qualified Prelude as P

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State hiding (state)
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Ratio
import Data.Sequence as S
import Data.Text hiding (head, foldl, foldr, map, tail)
import qualified Data.Text.IO as IO
import Numeric (fromRat)

import Lisp.Data
import qualified Lisp.SymbolTable as ST

runLispM :: LispM a -> LispState -> IO (Either LispError a, LispState)
runLispM comp = runStateT (runExceptT comp)

raise :: Text -> Text -> LispM a
raise sym msg = symToID sym >>= \id -> raise' id msg

raise' :: Int -> Text -> LispM a
raise' id msg = throwError $ LispError id msg

raiseFullSymbolTable :: LispM a
raiseFullSymbolTable = raise "internal-error" "Cannot create a new symbol"

raiseEmptyStack :: LispM a
raiseEmptyStack =
  raise "internal-error" $ "Called pop with empty stack"

raiseUnsetSymbol :: Int -> LispM a
raiseUnsetSymbol id = raise "internal-error" $ "Cannot find symbol with ID: " <> pack (show id)

raiseNoScope :: LispM a
raiseNoScope = raise "internal-error" $ "No local scope"

raiseUndefinedValue :: Text -> LispM a
raiseUndefinedValue name =
  raise "undefined-value" $ "`" <> name <> "` is not in scope"

raiseArgMismatch :: Int -> Int -> LispM a
raiseArgMismatch expected given =
  raise "arg-mismatch" $ "Expected " <> pack (show expected)
                      <> " args but " <> pack (show given)
                      <> " were given"

raiseTypeMismatch :: Text -> Value -> LispM a
raiseTypeMismatch typ val = do
  shown <- display val
  raise "type-mismatch" $ "Expected a `" <> typ <> "`, got: `" <> shown <> "`"

raiseInvalidRecur :: LispM a
raiseInvalidRecur = raise "invalid-recur" $ "Cannot recur outside of a lambda"

raiseCompileDottedList :: Value -> LispM a
raiseCompileDottedList value = do
  displayed <- display value
  raise "compile-dotted-list" $
    "Cannot compile a dotted list: " <> displayed

raiseDisallowedForm :: Text -> LispM a
raiseDisallowedForm name =
  raise "disallowed-form" $ "Cannot call `" <> name <> "` here"

raiseParseError :: Text -> LispM a
raiseParseError = raise "parse-error"

raiseIndexOutOfBounds :: Int -> Int -> LispM a
raiseIndexOutOfBounds given expected =
  raise "index-out-of-bounds" $ "Tried to access index " <> pack (show given)
                            <> " when maximum index is " <> pack (show expected)

raiseInvalidLet :: Value -> LispM a
raiseInvalidLet val = do
  displayed <- display val
  raise "invalid-let" $ "Invalid let form: " <> displayed

raiseInvalidSyntaxQuote :: Text -> LispM a
raiseInvalidSyntaxQuote = raise "invalid-syntax-quote"

symToID :: Text -> LispM Int
symToID text = do
  state <- get
  case ST.symToID text $ symbolTable state of
    Nothing -> raiseFullSymbolTable
    Just (sym, table) -> do
      put $ state { symbolTable = table }
      return sym

idToSym :: Int -> LispM Text
idToSym id = gets (ST.idToSym id . symbolTable)
         >>= maybe (raiseUnsetSymbol id) return

symbol :: Text -> LispM Value
symbol text = Symbol <$> symToID text

lookupSymbol :: Int -> LispM Value
lookupSymbol id = lookupSymbol' id
              >>= maybe (idToSym id >>= raiseUndefinedValue) return

lookupSymbol' :: Int -> LispM (Maybe Value)
lookupSymbol' id =
  gets $ \state ->
    let local = foldl (<|>) Nothing . map (IM.lookup id) $ scope state
        global = IM.lookup id $ globals state
    in  local <|> global

modifyScope :: ([Env] -> LispM (a, [Env])) -> LispM a
modifyScope f = do
  state <- get
  (ret, scope') <- f $ scope state
  put $ state { scope = scope' }
  return ret

modifyStack :: (Seq Value -> LispM (a, Seq Value)) -> LispM a
modifyStack f = do
  state <- get
  (ret, stack') <- f $ stack state
  put $ state { stack = stack' }
  return ret

push :: Value -> LispM ()
push v = modifyStack $ \vs -> return ((), v <| vs)

pop :: LispM Value
pop = flip S.index 0 <$> popN 1

pop2 :: LispM (Value, Value)
pop2 = do
  [first, second] <- popN 2
  return (first, second)

popN :: Int -> LispM (Seq Value)
popN int = popNWith int (<|) S.empty

popNWith :: Int -> (Value -> a -> a) -> a -> LispM a
popNWith int f =
  let go 0 acc vals = return (acc, vals)
      go n acc vals =
        case viewl vals of
          EmptyL -> raiseEmptyStack
          (val :< vals') -> go (pred n) (f val acc) vals'
  in  modifyStack . go int

localDef :: Int -> Value -> LispM ()
localDef key val = localDef' [(key, val)]

localDef' :: IM.IntMap Value -> LispM ()
localDef' defs =
  modifyScope $ \envs -> do
    when (P.null envs) raiseNoScope
    return ((), IM.intersection defs (head envs) : tail envs)

globalDef :: Int -> Value -> LispM ()
globalDef key val =
  modify $ \state ->
    state { globals = IM.insert key val $ globals state }

globalDef' :: Text -> Value -> LispM ()
globalDef' key val = symToID key >>= flip globalDef val

matchArgs :: Seq Int -> Maybe Int -> Seq Value -> LispM (IM.IntMap Value)
matchArgs ids Nothing args
  | S.length ids == S.length args = return . foldr (uncurry IM.insert) IM.empty $ S.zip ids args
  | otherwise = raiseArgMismatch (S.length ids) (S.length args)
matchArgs ids (Just id) args
  | S.length ids <= S.length args =
    let (args', rest) = S.splitAt (S.length ids) args
    in  return . foldr (uncurry IM.insert) IM.empty  $ S.zip (ids |> id) (args' |> list rest)
  | otherwise = raiseArgMismatch (S.length ids) (S.length args)

defmacro :: Text -> (Seq Value -> LispM (Seq Instruction)) -> LispM ()
defmacro sym func = globalDef' sym $ Macro (Left (sym, func)) []

defmacroN :: Int -> Text -> (Seq Value -> LispM (Seq Instruction)) -> LispM ()
defmacroN n sym func =
  defmacro sym $ \args -> do
    when (S.length args /= n) $ raiseArgMismatch n (S.length args)
    func args

defmacro0 :: Text -> LispM (Seq Instruction) -> LispM ()
defmacro0 sym func = defmacroN 0 sym $ const func

defmacro1 :: Text -> (Value -> LispM (Seq Instruction)) -> LispM ()
defmacro1 sym func = defmacroN 1 sym $ \[x] -> func x

defmacro2 :: Text -> (Value -> Value -> LispM (Seq Instruction)) -> LispM ()
defmacro2 sym func = defmacroN 2 sym $ \[x, y] -> func x y

defmacro3 :: Text -> (Value -> Value -> Value -> LispM (Seq Instruction)) -> LispM ()
defmacro3 sym func = defmacroN 3 sym $ \[x, y, z] -> func x y z

defun :: Text -> (Seq Value -> LispM Value) -> LispM ()
defun sym func = globalDef' sym $ Lambda (Left (sym, func)) []

defunN :: Int -> Text -> (Seq Value -> LispM Value) -> LispM ()
defunN n sym func =
  defun sym $ \args -> do
    when (S.length args /= n) $ raiseArgMismatch n (S.length args)
    func args

defun0 :: Text -> LispM Value -> LispM ()
defun0 sym func = defunN 0 sym $ const func

defun1 :: Text -> (Value -> LispM Value) -> LispM ()
defun1 sym func = defunN 1 sym $ \[x] -> func x

defun2 :: Text -> (Value -> Value -> LispM Value) -> LispM ()
defun2 sym func = defunN 2 sym $ \[x, y] -> func x y

defun3 :: Text -> (Value -> Value -> Value -> LispM Value) -> LispM ()
defun3 sym func = defunN 3 sym $ \[x, y, z] -> func x y z

printVal :: Value -> LispM ()
printVal = liftIO . IO.putStrLn <=< display

display :: Value -> LispM Text
display Nil = return "()"
display (Number val)
  | denominator val == 1 = return . pack . show $ numerator val
  | otherwise = return . pack . show $ (fromRat val :: Double)
display (Symbol id) = idToSym id
display (String str)  = return $ "\"" <> str <> "\""
display (Quote val)  = mappend "'" <$> display val
display (List x xs)  = do
  texts <- mapM display . F.toList $ x <| xs
  return $ "(" <> intercalate " " texts <> ")"
display (DottedList x xs y) = do
  texts <- mapM display . F.toList $ x <| xs
  text <- display y
  return $ "(" <> intercalate " " texts <> " . " <> text <> ")"
display (Lambda (Left (n, _)) _) = do
  return $ "#<native function: " <> n <> ">"
display (Lambda (Right (CompiledFunction _ _ _ src)) _) = do
  displayed <- display src
  return $ "#<" <> displayed <> ">"
display (Macro (Left (n, _)) _) = do
  return $ "#<native macro: " <> n <> ">"
display (Macro (Right (CompiledFunction _ _ _ src)) _) = do
  displayed <- display src
  return $ "#<" <> displayed <> ">"
display (Error (LispError typ msg)) = do
  sym <- idToSym typ
  return $ "#<error " <> sym  <> ": " <> msg <> ">"
