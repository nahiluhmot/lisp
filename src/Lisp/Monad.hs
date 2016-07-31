{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Lisp.Monad where

import Prelude hiding (id)

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State hiding (state)
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import Data.Ratio
import qualified Data.Sequence as S
import Data.Text hiding (foldr)
import qualified Data.Text.IO as IO
import Numeric (fromRat)

import Lisp.Data
import qualified Lisp.SymbolTable as ST

runLispM :: LispM a -> LispState -> IO (Either LispError a, LispState)
runLispM comp = runStateT (runExceptT comp)

symToID :: Text -> LispM Int
symToID text = do
  state <- get
  case ST.symToID text $ symbolTable state of
    Nothing -> throwError FullIndex
    Just (sym, table) -> do
      put $ state { symbolTable = table }
      return sym

idToSym :: Int -> LispM Text
idToSym id = gets (ST.idToSym id . symbolTable)
         >>= maybe (throwError $ UnsetSymbol id) return

symbol :: Text -> LispM Value
symbol text = Symbol <$> symToID text

lookupSymbol :: Int -> LispM Value
lookupSymbol id = do
  state <- get
  let local = foldr (\env acc -> acc <|> IM.lookup id env) Nothing $ scope state
      global = IM.lookup id $ globals state
      found = local <|> global
  maybe (idToSym id >>= throwError . UndefinedValue) return found

modifyScope :: (S.Seq Env -> LispM (a, S.Seq Env)) -> LispM a
modifyScope f = do
  envs <- gets scope
  (ret, envs') <- f envs
  modify $ \state -> state { scope = envs' }
  return ret

modifyStack :: (S.Seq Value -> LispM (a, S.Seq Value)) -> LispM a
modifyStack f = do
  vals <- gets stack
  (ret, stack') <- f vals
  modify $ \state -> state { stack = stack' }
  return ret

push :: Value -> LispM ()
push v = modifyStack $ \vs -> return ((), v S.<| vs)

pop :: LispM Value
pop =
  modifyStack $ \vs ->
    case S.viewl vs of
      S.EmptyL -> throwError EmptyStack
      (v S.:< vs') -> return (v, vs')

popN :: Int -> LispM (S.Seq Value)
popN int =
  modifyStack $ \vs -> do
    when (S.length vs < int) $ throwError EmptyStack
    return $ S.splitAt int vs

localDef :: Int -> Value -> LispM ()
localDef key val = localDef' $ (key, val) : []

localDef' :: Foldable f => f (Int, Value) -> LispM ()
localDef' defs =
  let insertValues S.EmptyL = throwError NoScope
      insertValues (env S.:< envs') = return ((), foldr (uncurry IM.insert) env defs S.<| envs')
  in  modifyScope (insertValues . S.viewl)

globalDef :: Int -> Value -> LispM ()
globalDef key val =
  modify $ \state ->
    state { globals = IM.insert key val $ globals state }

globalDef' :: Text -> Value -> LispM ()
globalDef' key val = symToID key >>= flip globalDef val

printVal :: Value -> LispM ()
printVal = liftIO . IO.putStrLn <=< display

display :: Value -> LispM Text
display Nil = return "nil"
display (Number val)
  | denominator val == 1 = return . pack . show $ numerator val
  | otherwise = return . pack . show $ (fromRat val :: Double)
display (Symbol id) = idToSym id
display (String str)  = return $ "\"" `mappend` str `mappend` "\""
display (Quote val)  = (mappend "'") <$> display val
display c@(Cons _ _)  =
  case toSeq c of
    Right xs -> do
      texts <- mapM display (F.toList xs)
      return $ "(" `mappend` intercalate " " texts `mappend` ")"
    Left (xs, x) -> do
      texts <- mapM display (F.toList xs)
      text <- display x
      return $ "(" `mappend` intercalate " " texts `mappend` " . " `mappend` text `mappend` ")"
display (Lambda (Left (n, _)) _) = do
  return $ "#<native function: " `mappend` n `mappend` ">"
display (Lambda (Right (CompiledFunction _ _ _ src)) _) = do
  displayed <- display src
  return $ "#<" `mappend` displayed `mappend` ">"
display (Macro (Left (n, _)) _) = do
  return $ "#<native macro: " `mappend` n `mappend` ">"
display (Macro (Right (CompiledFunction _ _ _ src)) _) = do
  displayed <- display src
  return $ "#<" `mappend` displayed `mappend` ">"

eq :: Value -> Value -> Bool
eq Nil Nil = True
eq (Number x) (Number y) = x == y
eq (Symbol x) (Symbol y) = x == y
eq (String x) (String y) = x == y
eq (Quote x) (Quote y) = x `eq` y
eq (Cons x y) (Cons x' y') = (x `eq` x') && (y `eq` y')
eq _ _ = False

typeOf :: Value -> LispM Value
typeOf Nil = Symbol <$> symToID "nil"
typeOf (Number _) = Symbol <$> symToID "number"
typeOf (Symbol _) = Symbol <$> symToID "symbol"
typeOf (String _) = Symbol <$> symToID "string"
typeOf (Quote _) = Symbol <$> symToID "quote"
typeOf (Cons _ _) = Symbol <$> symToID "cons"
typeOf (Lambda _ _) = Symbol <$> symToID "lambda"
typeOf (Macro _ _) = Symbol <$> symToID "macro"
