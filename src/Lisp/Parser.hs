{-# LANGUAGE OverloadedStrings #-}

module Lisp.Parser where

import Control.Monad.State hiding (state)
import Control.Monad.Except
import Data.Char (digitToInt)
import Data.Functor
import Data.Sequence
import Data.Text hiding (foldl, foldr, map)
import Text.Parsec as P

import Lisp.Data
import qualified Lisp.Monad as M

type Parser = ParsecT Text Bool LispM

parse :: Text -> LispM (Seq Value)
parse input = do
  result <- runParserT (values <* eof) False "*repl*" input
  case result of
    Left err -> throwError $ ParseError err
    Right val -> return val

values :: Parser (Seq Value)
values = fromList <$> (spaces *> many (value <* spaces))

value :: Parser Value
value = choice $ map try [ num
                         , symbol
                         , list
                         , str
                         , quoted
                         , dottedList
                         , syntaxQuoted
                         , syntaxUnquoted
                         , syntaxSplatted
                         ]

syntaxQuoted :: Parser Value
syntaxQuoted = do
  inSyntaxQuoted <- getState
  when inSyntaxQuoted $ parserFail "Nested syntax quotes unsupported"
  parsed <- char '`' *> spaces *> putState True *> value <* putState False
  syntaxQuote <- lift $ M.symbol "syntax-quote"
  return $ Cons syntaxQuote (Cons parsed Nil)

syntaxUnquoted :: Parser Value
syntaxUnquoted = do
  isSyntaxQuoted <- getState
  unless isSyntaxQuoted $ parserFail "Cannot unquote outside of syntax quote"
  Cons <$> (lift $ M.symbol "unquote")
       <*> (Cons <$> (char ',' *> spaces *> putState False *> value <* putState True)
                 <*> pure Nil)

syntaxSplatted :: Parser Value
syntaxSplatted = do
  isSyntaxQuoted <- getState
  unless isSyntaxQuoted $ parserFail "Cannot unquote-splat outside of syntax quote"
  Cons <$> (lift $ M.symbol "unquote-splat")
       <*> (Cons <$> (string ",@" *> spaces *> putState False *> value <* putState True)
                 <*> pure Nil)

quoted :: Parser Value
quoted = Cons <$> lift (M.symbol "quote")
              <*> (Cons <$> (char '\'' *> spaces *> value) <*> pure Nil)

list :: Parser Value
list = foldr Cons Nil <$> between (char '(')  (char ')') values

dottedList :: Parser Value
dottedList =
  fmap (uncurry $ flip (foldr Cons)) .
    between (char '(') (char ')') $
      (,) <$> values <* spaces <* dot <* spaces
          <*> value <* spaces

symbol :: Parser Value
symbol = do
  ((:) <$> (letter <|> allowedSymbol) <*> many (alphaNum <|> allowedSymbol <|> dot)) >>=
    (lift . M.symbol . pack)

str :: Parser Value
str = String . pack <$> (char '"' *> many (noneOf ['"']) <* char '"')

num :: Parser Value
num = do
  isPos <- option True $ char '-' $> False
  n <- foldl (\acc x -> (acc * 10) + x) 0 <$> ints
  let n' | isPos = n
         | otherwise = -1 * n
  result <- optionMaybe $ char '.' *> (foldr (\x acc -> (acc + x) / 10) 0 <$> ints)
  case result of
    Nothing -> return $ Number n'
    Just frac
      | isPos -> return . Number $ n + frac
      | otherwise -> return . Number $ n' - frac

ints :: Num a => Parser [a]
ints = map (fromIntegral . digitToInt) <$> many1 digit

dot :: Parser Char
dot = char '.'

allowedSymbol :: Parser Char
allowedSymbol = oneOf "+-*/?!="
