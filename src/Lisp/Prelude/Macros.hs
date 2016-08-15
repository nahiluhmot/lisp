{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Lisp.Prelude.Macros (defPreludeMacros) where

import Prelude hiding (id, last)
import Control.Monad
import Data.Sequence as S
import Data.Foldable

import Lisp.Data
import Lisp.Core
import Lisp.Compiler

defPreludeMacros :: LispM ()
defPreludeMacros = do
  lambda <- symbol "lambda"
  defmacro "lambda" $ \vals -> do
    func <- compileFunc lambda vals
    return $ [MakeLambda func]

  mac <- symbol "macro"
  defmacro "macro" $ \vals -> do
    func <- compileFunc mac vals
    return $ [MakeMacro func]

  defmacro1 "return" $ \val -> (|> Return) <$> compile' val

  defmacro2 "def" $ \name val ->
    case name of
      Symbol sym -> (|> Def sym) <$> compile' val
      val' -> raiseTypeMismatch "symbol" val'

  defmacro "do" compileValues
  defmacro "if" compileIf
  defmacro "recur" compileRecur
  defmacro "let" compileLet
  defmacro1 "quote" compileQuote
  defun1 "quotef" $ return . Quote
  defmacro1 "syntax-quote" compileSyntaxQuote

compileFunc :: Value -> Seq Value -> LispM CompiledFunction
compileFunc sym vals = do
  let namesToIDs = mapM unSymbol
  case viewl vals of
    EmptyL -> raiseArgMismatch 1 0
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
  when (S.null vals) $ raiseArgMismatch 2 0
  let (defs :< body) = viewl vals
      go sexp =
        case toSeq sexp of
          Right [Symbol id, value] -> (|> Set id) <$> compile' value
          _ -> raiseInvalidLet sexp
  case toSeq defs of
    Right conses -> do
      bodyInsns <- compileValues body
      (PushScope <|) <$> foldrM (\def acc -> (>< acc) <$> go def) (bodyInsns |> PopScope) conses
    Left _ -> raiseInvalidLet defs

compileRecur :: Seq Value -> LispM (Seq Instruction)
compileRecur args = (|> Recur (S.length args)) <$> compileValues' args

compileIf :: Seq Value -> LispM (Seq Instruction)
compileIf vals
  | S.length vals < 2 = raiseArgMismatch 2 (S.length vals)
  | otherwise = do
      let ([cond, body], rest) = S.splitAt 2 vals
      condition <- compile' cond
      thenCase <- compile body
      elseCase <- compileValues rest
      let elseCase' | S.null elseCase = [Push Nil]
                    | otherwise = elseCase
      return $ (condition |> BranchUnless (2 + S.length thenCase))
            >< (thenCase |> Jump (succ $ S.length elseCase'))
            >< elseCase'

compileQuote :: Value -> LispM (Seq Instruction)
compileQuote val = singleton . Push <$> compileQuote' val

compileQuote' :: Value -> LispM Value
compileQuote' val = do
  quote <- symbol "quote"
  let go vals@(List car cdr)
        | (car == quote) && (S.length cdr == 1) = Quote <$> go (index cdr 0)
        | otherwise = return vals
      go curr = return curr
  go val

compileSyntaxQuote :: Value -> LispM (Seq Instruction)
compileSyntaxQuote arg = do
  quote <- symbol "quote"
  unquote <- symbol "unquote"
  splat <- symbol "unquote-splat"
  cons <- symToID "cons"
  append <- symToID "append"
  quoteFirst <- symToID "quote-first"
  let quotef = function quoteFirst $ \vals -> do
        when (S.length vals /= 1) $ raiseArgMismatch 1 (S.length vals)
        case S.index vals 0 of
          (List car []) -> return $ Quote car
          val -> raiseTypeMismatch "list with one element" val
  let go (List car cdr)
        | car == unquote =
          case cdr of
            [val] -> Right <$> compile val
            _ -> recur
        | car == splat =
          case cdr of
            [val] -> Left <$> compile val
            _ -> recur
        | car == quote && (S.length cdr == 1) = do
            result <- go $ List (S.index cdr 0) []
            case result of
              Left val -> return . Left $ Push quotef <| (val |> Funcall 1)
              Right val -> return . Right $ Push quotef <| (val |> Funcall 1)
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
        raiseInvalidSyntaxQuote "Cannot unquote-splat in cdr postion"
  result <- go arg
  case result of
    Right insns -> return insns
    Left _ -> raiseInvalidSyntaxQuote "Cannot unquote-splat outside of cons"

unSymbol :: Value -> LispM Int
unSymbol (Symbol id) = return id
unSymbol val = raiseTypeMismatch "symbol" val
