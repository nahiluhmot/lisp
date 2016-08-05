{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Lisp.Core.Macros (defCoreMacros) where

import Prelude hiding (id, last)
import Control.Monad.Except
import Data.Sequence as S
import Data.Foldable

import Lisp.Data
import Lisp.Monad
import Lisp.Compiler

defCoreMacros :: LispM ()
defCoreMacros = do
  lambda <- symbol "lambda"
  defmacro "lambda" $ \vals -> do
    func <- compileFunc lambda vals
    return $ [MakeLambda func]

  macro <- symbol "macro"
  defmacro "macro" $ \vals -> do
    func <- compileFunc macro vals
    return $ [MakeMacro func]

  defmacro1 "return" $ \val -> (|> Return) <$> compile' val

  defmacro2 "def" $ \name val ->
    case name of
      Symbol sym -> (|> Def sym) <$> compile' val
      _ -> throwError $ TypeMismatch "symbol"

  defmacro "if" compileIf
  defmacro "recur" compileRecur
  defmacro "let" compileLet
  defmacro1 "quote" compileQuote
  defmacro1 "syntax-quote" compileSyntaxQuote

compileFunc :: Value -> Seq Value -> LispM CompiledFunction
compileFunc sym vals = do
  let namesToIDs = mapM unSymbol
  case viewl vals of
    EmptyL -> throwError $ ArgMismatch 1 0
    (args :< body) -> do
      (ids, extra) <-
        case toSeq args of
          (Left (xs, x)) -> (,) <$> namesToIDs xs <*> (Just <$> unSymbol x)
          (Right xs) -> (, Nothing) <$> namesToIDs xs
      insns <- compileValues body
      return $ CompiledFunction { instructions = insns
                                , argIDs = ids
                                , extraArgsID = extra
                                , source = List sym vals
                                }

compileLet :: Seq Value -> LispM (Seq Instruction)
compileLet vals = do
  when (S.null vals) $ throwError $ ArgMismatch 2 0
  let (defs :< body) = viewl vals
      go sexp =
        case toSeq sexp of
          Right [Symbol id, value] -> (|> Set id) <$> compile' value
          _ -> display sexp >>= throwError . InvalidLet
  case toSeq defs of
    Right conses -> do
      bodyInsns <- compileValues body
      (PushScope <|) <$> foldrM (\def acc -> (>< acc) <$> go def) (bodyInsns |> PopScope) conses
    Left _ -> display defs >>= throwError . InvalidLet

compileRecur :: Seq Value -> LispM (Seq Instruction)
compileRecur args = (|> Recur (S.length args)) <$> compileValues' args

compileIf :: Seq Value -> LispM (Seq Instruction)
compileIf vals
  | S.length vals < 2 = throwError $ ArgMismatch 2 (S.length vals)
  | otherwise = do
      let ([cond, body], rest) = S.splitAt 2 vals
      condition <- compile' cond
      thenCase <- compile body
      elseCase <- compileValues rest
      return $ (condition |> BranchUnless (2 + S.length thenCase))
            >< (thenCase |> Jump (succ $ S.length elseCase))
            >< elseCase

compileQuote :: Value -> LispM (Seq Instruction)
compileQuote val = singleton . Push <$> compileQuote' val

compileQuote' :: Value -> LispM Value
compileQuote' val = do
  quote <- symbol "quote"
  let go vals@(List car cdr)
        | (car /= quote) && (S.length cdr /= 1) = return vals
        | S.length cdr == 1 = Quote <$> go (index cdr 0)
      go curr = return curr
  go val

compileSyntaxQuote :: Value -> LispM (Seq Instruction)
compileSyntaxQuote arg = do
  unquote <- symbol "unquote"
  splat <- symbol "unquote-splat"
  cons <- symToID "cons"
  append <- symToID "append"
  let go (List car cdr)
        | car == unquote =
          case cdr of
            [val] -> Right <$> compile val
            _ -> recur
        | car == splat =
          case cdr of
            [val] -> Left <$> compile val
            _ -> recur
        | otherwise = recur
        where recur = mapM go (car <| cdr) >>= foldrM combine (Right [Push Nil])
      go (DottedList car cdr last) = do
        first <- mapM go (car <| cdr)
        rest <- go last
        foldrM combine rest first
      go val = Right <$> compileQuote val
      combine (Right car') (Right cdr') =
        return . Right $ Get cons <| ((car' >< cdr') |> Funcall 2)
      combine (Left car') (Right cdr') =
        return . Right $ Get append <| ((car' >< cdr') |> Funcall 2)
      combine _ _ =
        throwError $ InvalidSyntaxQuote "Cannot unquote-splat in cdr postion"
  result <- go arg
  case result of
    Right insns -> return insns
    Left _ -> throwError $ InvalidSyntaxQuote "Cannot unquote-splat outside of cons"

unSymbol :: Value -> LispM Int
unSymbol (Symbol id) = return id
unSymbol _ = throwError $ TypeMismatch "symbol"