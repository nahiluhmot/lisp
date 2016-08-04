{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Lisp.Monad where

import Prelude hiding (id)

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State hiding (state)
import qualified Data.IntMap as IM
import Data.Sequence as S
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
          (scope state |> globals state)

modifyScope :: (Seq Env -> LispM (a, Seq Env)) -> LispM a
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

popN :: Int -> LispM (Seq Value)
popN int =
  let go 0 before after = return (before, after)
      go _ _ [] = throwError EmptyStack
      go n before after =
        let (val :< after') = viewl after
        in  go (pred n) (val <| before) after'
  in  modifyStack $ go int []

localDef :: Int -> Value -> LispM ()
localDef key val = localDef' [(key, val)]

localDef' :: Seq (Int, Value) -> LispM ()
localDef' defs =
  let insertValues EmptyL = throwError NoScope
      insertValues (env :< envs') =
        return ((), foldr (uncurry IM.insert) env defs <| envs')
  in  modifyScope (insertValues . viewl)

globalDef :: Int -> Value -> LispM ()
globalDef key val =
  modify $ \state ->
    state { globals = IM.insert key val $ globals state }

globalDef' :: Text -> Value -> LispM ()
globalDef' key val = symToID key >>= flip globalDef val

matchArgs :: Seq Int -> Maybe Int -> Seq Value -> LispM (Seq (Int, Value))
matchArgs ids Nothing args
  | S.length ids == S.length args = return $ S.zip ids args
  | otherwise = throwError $ ArgMismatch (S.length ids) (S.length args)
matchArgs ids (Just id) args
  | S.length ids <= S.length args =
    let (args', rest) = S.splitAt (S.length ids) args
    in  return $ S.zip (ids |> id) (args' |> list rest)
  | otherwise = throwError $ ArgMismatch (S.length ids) (S.length args)

defmacro :: Text -> (Seq Value -> LispM (Seq Instruction)) -> LispM ()
defmacro sym func = globalDef' sym $ Macro (Left (sym, func)) []

defmacroN :: Int -> Text -> (Seq Value -> LispM (Seq Instruction)) -> LispM ()
defmacroN n sym func =
  defmacro sym $ \args -> do
    when (S.length args /= n) $ throwError $ ArgMismatch n (S.length args)
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
    when (S.length args /= n) $ throwError $ ArgMismatch n (S.length args)
    func args

defun0 :: Text -> LispM Value -> LispM ()
defun0 sym func = defunN 0 sym $ const func

defun1 :: Text -> (Value -> LispM Value) -> LispM ()
defun1 sym func = defunN 1 sym $ \[x] -> func x

defun2 :: Text -> (Value -> Value -> LispM Value) -> LispM ()
defun2 sym func = defunN 2 sym $ \[x, y] -> func x y

defun3 :: Text -> (Value -> Value -> Value -> LispM Value) -> LispM ()
defun3 sym func = defunN 3 sym $ \[x, y, z] -> func x y z
