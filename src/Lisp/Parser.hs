{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.Parser (parse) where

import Control.Monad.State hiding (state)
import Data.Char (digitToInt)
import Data.Functor
import Data.Sequence as S
import Data.Text hiding (foldl, foldr, map)
import Text.Parsec as P hiding (parse)

import Lisp.Data hiding (list, dottedList)
import qualified Lisp.Data as D
import qualified Lisp.Monad as M

type Parser = ParsecT Text Bool LispM

parse :: Text -> LispM (Seq Value)
parse input = runParserT (values <* eof) False "*repl*" input
          >>= either (M.raiseParseError . pack . show) return

values :: Parser (Seq Value)
values = fromList <$> (spaces *> many1 (value <* spaces))

value :: Parser Value
value = choice $ map try parsers

parsers :: [Parser Value]
parsers = [ num
          , symbol
          , nil
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
  syntaxQuote <- lift $ M.symbol "syntax-quote"
  parsed <- char '`' *> spaces *> putState True *> value <* putState False
  return $ List syntaxQuote [parsed]

syntaxUnquoted :: Parser Value
syntaxUnquoted = do
  isSyntaxQuoted <- getState
  unless isSyntaxQuoted $ parserFail "Cannot unquote outside of syntax quote"
  unquote <- lift $ M.symbol "unquote"
  parsed <- char ',' *> spaces *> putState False *> value <* putState True
  return $ List unquote [parsed]

syntaxSplatted :: Parser Value
syntaxSplatted = do
  isSyntaxQuoted <- getState
  unless isSyntaxQuoted $ parserFail "Cannot unquote-splat outside of syntax quote"
  unquoteSplat <- lift $ M.symbol "unquote-splat"
  parsed <- string ",@" *> spaces *> putState False *> value <* putState True
  return $ List unquoteSplat [parsed]

quoted :: Parser Value
quoted = List <$> lift (M.symbol "quote")
              <*> (S.singleton <$> (char '\'' *> spaces *> value))

nil :: Parser Value
nil = char '(' *> spaces *> char ')' $> Nil

list :: Parser Value
list = D.list <$> between (char '(')  (char ')') values

dottedList :: Parser Value
dottedList =
  between (char '(') (char ')') $
    D.dottedList <$> values <* dot
                 <*> between spaces spaces value

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
allowedSymbol = oneOf "+-*/?!=<>"
