{-# LANGUAGE OverloadedStrings #-}
module Lisp.Monad where

import Prelude hiding (id)

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State hiding (state)
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Ratio
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import Data.Text hiding (foldr)
import Data.Text.IO as IO

import Lisp.Data
import qualified Lisp.Index as I
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
  es <- envs
  let found = foldr (\env acc -> acc <|> IM.lookup id env) Nothing es
  maybe (idToSym id >>= throwError . UndefinedValue) return found

lookupContext :: Int -> LispM Context
lookupContext id = gets (I.lookup id . contexts)
             >>= maybe (throwError $ NoSuchContext id) return

lookupScope :: Int -> LispM Env
lookupScope id = gets (I.lookup id . scopes)
             >>= maybe (throwError $ NoSuchScope id) return

currentContext :: LispM (Int, Context)
currentContext = do
  id <- gets context
  ctx <- lookupContext id
  return (id, ctx)

callers :: LispM (S.Seq Context)
callers = fmap (callerIDs . snd) currentContext >>= T.mapM lookupContext

stack :: LispM (S.Seq Value)
stack = fmap (valStack . snd) currentContext

envs :: LispM (S.Seq Env)
envs = (S.|>) <$> (fmap (envIDs . snd) currentContext >>= T.mapM lookupScope)
              <*> gets globals

modifyContext :: (Context -> LispM (a, Context)) -> LispM a
modifyContext f = do
  (id, ctx) <- currentContext
  (ret, ctx') <- f ctx
  state <- get
  case I.update id ctx' $ contexts state of
    Nothing -> throwError $ NoSuchContext id
    Just contexts' -> do
      put $ state { contexts = contexts' }
      return ret

modifyStack :: (S.Seq Value -> LispM (a, S.Seq Value)) -> LispM a
modifyStack f =
  modifyContext $ \ctx@(Context _ _ vs) -> do
    (ret, vs') <- f vs
    return $ (ret, ctx { valStack = vs' })

push :: Value -> LispM ()
push v = do
  modifyStack $ \vs -> return ((), v S.<| vs)

pop :: LispM Value
pop = do
  ret <- modifyStack $ \vs ->
    case S.viewl vs of
      S.EmptyL -> throwError EmptyStack
      (v S.:< vs') -> do
        return (v, vs')
  return ret

popN :: Int -> LispM (S.Seq Value)
popN int = do
  ret <- modifyStack $ \vs -> do
    when (S.length vs < int) $ throwError EmptyStack
    return $ S.splitAt int vs
  return ret

localDef :: Int -> Value -> LispM ()
localDef key val = do
  (_, Context ids _ _) <- currentContext
  case S.viewl ids of
    S.EmptyL -> throwError NoScope
    (scopeID S.:< _) -> do
      scope <- lookupScope scopeID
      state <- get
      case I.update scopeID (IM.insert key val scope) $ scopes state of
        Nothing -> throwError FullIndex
        Just scopes' -> put $ state { scopes = scopes' }

localDef' :: Foldable f => f (Int, Value) -> LispM ()
localDef' defs = do
  (_, Context ids _ _) <- currentContext
  case S.viewl ids of
    S.EmptyL -> throwError NoScope
    (scopeID S.:< _) -> do
      scope <- lookupScope scopeID
      state <- get
      case I.update scopeID (foldr (uncurry IM.insert) scope defs) $ scopes state of
        Nothing -> throwError FullIndex
        Just scopes' -> put $ state { scopes = scopes' }

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
display (Number val) = return . pack . show $ numerator val
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
display (Lambda (Function _ _ _ src) _) = do
  case src of
    Left builtin -> return $ "<builtin function: " `mappend` builtin `mappend` ">"
    Right sexp -> do
      displayed <- display sexp
      return $ "#<" `mappend` displayed `mappend` ">"
display (Macro (Function _ _ _ src) _) = do
  case src of
    Left builtin -> return $ "<builtin macro: " `mappend` builtin `mappend` ">"
    Right sexp -> do
      displayed <- display sexp
      return $ "#<" `mappend` displayed `mappend` ">"

typeOf :: Value -> LispM Value
typeOf Nil = Symbol <$> symToID "nil"
typeOf (Number _) = Symbol <$> symToID "number"
typeOf (Symbol _) = Symbol <$> symToID "symbol"
typeOf (String _) = Symbol <$> symToID "string"
typeOf (Quote _) = Symbol <$> symToID "quote"
typeOf (Cons _ _) = Symbol <$> symToID "cons"
typeOf (Lambda _ _) = Symbol <$> symToID "lambda"
typeOf (Macro _ _) = Symbol <$> symToID "macro"

addBuiltin :: Text -> (S.Seq Value -> LispM (S.Seq Instruction)) -> LispM ()
addBuiltin name func = do
  id <- symToID name
  modify $ \state -> state { builtins = IM.insert id func (builtins state) }

lookupBuiltin :: Int -> LispM (Maybe (S.Seq Value -> LispM (S.Seq Instruction)))
lookupBuiltin id = gets $ IM.lookup id . builtins

gc :: LispM ()
gc = do
  (scopeIDs, contextIDs) <- usedScopesAndContexts
  state <- get
  let scopes' = I.select scopeIDs $ scopes state
      contexts' = I.select contextIDs $ contexts state
      nextGC' = 2 * max (nextGC state) (fromIntegral (I.size scopes') + fromIntegral (I.size contexts'))
      state' = state { scopes = scopes'
                     , contexts = contexts'
                     , nextGC = nextGC'
                     }
  put state'

gcIfNecessary :: LispM ()
gcIfNecessary = do
  total <- totalScopesAndContexts
  next <- gets nextGC
  when (total >= next) gc

totalScopesAndContexts :: LispM Integer
totalScopesAndContexts =
  gets $ \state -> fromIntegral (I.size (contexts state)) + fromIntegral (I.size (scopes state))

usedScopesAndContexts :: LispM (IS.IntSet, IS.IntSet)
usedScopesAndContexts =
  let go ctxID scopeIDs ctxIDs = do
        ctx <- lookupContext ctxID
        let findEnvIDs (Lambda _ ids) = ids
            findEnvIDs _ = S.empty
            scopeIDs' =
              F.foldr (\val acc -> F.foldr IS.insert acc (findEnvIDs val))
                      (F.foldr IS.insert scopeIDs (envIDs ctx))
                      (valStack ctx)
            ctxIDs' = IS.insert ctxID ctxIDs
        F.foldrM (uncurry . go)
                 (scopeIDs', ctxIDs')
                 (S.filter (flip IS.notMember ctxIDs) $ callerIDs ctx)
  in  gets context >>= \id -> go id IS.empty IS.empty
