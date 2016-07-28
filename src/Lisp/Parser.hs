module Lisp.Parser where

import Control.Monad.State hiding (state)
import Control.Monad.Except
import Data.Char (digitToInt)
import Data.Text hiding (foldl, foldr, map)
import Text.Parsec as P
import Data.Sequence

import Lisp.Data
import qualified Lisp.Monad as M

type Parser = ParsecT Text () LispM

parse :: Text -> LispM (Seq Value)
parse input = do
  result <- runParserT (values <* eof) () "*repl*" input
  case result of
    Left err -> throwError $ ParseError err
    Right val -> return val

values :: Parser (Seq Value)
values = fromList <$> (spaces *> many (value <* spaces))

value :: Parser Value
value = choice $ map try [ symbol
                         , list
                         , str
                         , num
                         , quoted
                         , dottedList
                         ]

quoted :: Parser Value
quoted = Quote <$> (char '\'' *> spaces *> value)

list :: Parser Value
list = foldr Cons Nil <$> between (char '(')  (char ')') values

dottedList :: Parser Value
dottedList =
  fmap (uncurry $ flip (foldr Cons)) .
    between (char '(') (char ')') $
      (,) <$> values <* spaces <* char  '.' <* spaces
          <*> value <* spaces

symbol :: Parser Value
symbol = do
  ((:) <$> (letter <|> allowedSymbol) <*> many (alphaNum <|> allowedSymbol)) >>=
    (lift . M.symbol . pack)

str :: Parser Value
str = String . pack <$> (char '"' *> many (noneOf ['"']) <* char '"')

num :: Parser Value
num =
  Number . foldl (\acc x -> (acc * 10) + x) 0
         . map (fromIntegral . digitToInt)
         <$> many1 digit

allowedSymbol :: Parser Char
allowedSymbol = oneOf "+-*/?!="
