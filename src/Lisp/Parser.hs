{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.Parser (parse, parseFile) where

import Control.Monad.State.Strict hiding (state)
import Data.Char (digitToInt)
import Data.Functor
import Data.Sequence as S
import Data.Text hiding (foldl, foldr, map)
import Data.Text.IO as IO
import Text.Parsec as P hiding (parse)

import Lisp.Data hiding (list, dottedList)
import qualified Lisp.Data as D
import qualified Lisp.Core as C

type Parser = ParsecT Text Bool LispM

parse :: Text -> LispM (Seq Value)
parse input = runParserT (values <* eof) False "*repl*" input
          >>= either (C.raiseParseError . pack . show) return

parseFile :: FilePath -> LispM (Seq Value)
parseFile path = do
  input <- liftIO $ IO.readFile path
  result <- runParserT (values <* eof) False path input
  either (C.raiseParseError . pack . show) return result

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
  _ <- char '`' *> spaces
  inSyntaxQuoted <- getState
  when inSyntaxQuoted $ parserFail "Nested syntax quotes unsupported"
  List <$> lift (C.symbol "syntax-quote")
       <*> (S.singleton <$> (putState True *> value <* putState False))

syntaxUnquoted :: Parser Value
syntaxUnquoted = do
  _ <- char ',' *> spaces
  isSyntaxQuoted <- getState
  unless isSyntaxQuoted $ parserFail "Cannot unquote outside of syntax quote"
  List <$> lift (C.symbol "unquote")
       <*> (S.singleton <$> (putState False *> value <* putState True))

syntaxSplatted :: Parser Value
syntaxSplatted = do
  _ <- string ",@" *> spaces
  isSyntaxQuoted <- getState
  unless isSyntaxQuoted $ parserFail "Cannot unquote-splat outside of syntax quote"
  List <$> lift (C.symbol "unquote-splat")
       <*> (S.singleton <$> (putState False *> value <* putState True))

quoted :: Parser Value
quoted = List <$> lift (C.symbol "quote")
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
    (lift . C.symbol . pack)

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
