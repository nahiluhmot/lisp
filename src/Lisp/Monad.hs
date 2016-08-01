{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Lisp.Monad where

import Prelude hiding (id)

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State hiding (state)
import qualified Data.IntMap as IM
import qualified Data.Sequence as S
import Data.Text hiding (foldl, foldr)

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
lookupSymbol id = lookupSymbol' id
              >>= maybe (idToSym id >>= throwError . UndefinedValue) return

lookupSymbol' :: Int -> LispM (Maybe Value)
lookupSymbol' id =
  gets $ \state ->
    foldl (\acc env -> acc <|> IM.lookup id env)
          Nothing
          (scope state S.|> globals state)

modifyScope :: (S.Seq Env -> LispM (a, S.Seq Env)) -> LispM a
modifyScope f = do
  state <- get
  (ret, scope') <- f $ scope state
  put $ state { scope = scope' }
  return ret

modifyStack :: (S.Seq Value -> LispM (a, S.Seq Value)) -> LispM a
modifyStack f = do
  state <- get
  (ret, stack') <- f $ stack state
  put $ state { stack = stack' }
  return ret

push :: Value -> LispM ()
push v = modifyStack $ \vs -> return ((), v S.<| vs)

pop :: LispM Value
pop =
  let go S.EmptyL = throwError EmptyStack
      go (v S.:< vs') = return (v, vs')
  in  modifyStack $ go . S.viewl

popN :: Int -> LispM (S.Seq Value)
popN int =
  modifyStack $ \vs -> do
    when (S.length vs < int) $ throwError EmptyStack
    return $ S.splitAt int vs

localDef :: Int -> Value -> LispM ()
localDef key val = localDef' [(key, val)]

localDef' :: S.Seq (Int, Value) -> LispM ()
localDef' defs =
  let insertValues S.EmptyL = throwError NoScope
      insertValues (env S.:< envs') =
        return ((), foldr (uncurry IM.insert) env defs S.<| envs')
  in  modifyScope (insertValues . S.viewl)

globalDef :: Int -> Value -> LispM ()
globalDef key val =
  modify $ \state ->
    state { globals = IM.insert key val $ globals state }

globalDef' :: Text -> Value -> LispM ()
globalDef' key val = symToID key >>= flip globalDef val
