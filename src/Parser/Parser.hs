module Parser.Parser
  ( module Parser.Parser
  , module Text.Trifecta
  )
  where

import Text.Trifecta hiding (Parser, unparser)
import Text.Trifecta qualified as T
import Text.Trifecta.Delta
import Data.Char     qualified as C
import Text.Parser.LookAhead
import Control.Monad.Reader
import Data.ByteString.Char8 qualified as B
import Data.Set qualified as S
import Debug.Trace


type Parser = ReaderT Int T.Parser

unparser =
  runReaderT `flip` 0

infixr 1 ?<
(?<) = flip (<?>)

lexeme spc p =
  p <* spc

lexeme' =
  lexeme . many

tok n p =
  n ?< lexeme' bspc p


ishspace c =
  C.isSpace c && c /= '\n' && c /= '\r'

hspace = "horizonal whitespace" ?< do
  satisfy ishspace

vspace = "vertical whitespace" ?< do
  newline


blockComment = "block comment" ?< void do
  string "{-"
  manyTill anyChar $ string "-}"

lineComment = "line comment" ?< void do
  string "--"
  manyTill anyChar $ lookAhead vspace

comment = "comment" ?<
  lineComment <|> blockComment


getIndent = do
  ss <- B.takeWhile ishspace <$> T.line
  pure $ B.length ss

getCol =
  fi . column <$> position


lineCont = "line continuation" ?< do
  x <- ask
  vspace
  many spc
  y <- getCol
  guard $ y >= x


spc = asum
  [ void space
  , comment
  ]

hspc = asum
  [ void hspace
  , comment
  ]

bspc = asum
  [ hspc
  , try lineCont
  ]

end = asum
  [ eof
  , void $ vspace *> many spc
  ]


scope f p = do
  loc <- ask
  a <- f <$> getCol
  local (const a) do
    "scope " <> show a ?< p

scoped =
  scope id

subscope p = do
  checkIndent
  scope (+1) p

checkIndent = do
  targ <- ask
  curr <- getCol
  guard (curr >= targ)


operator op p f = do
  l <- p
  (<|> pure l) $ try do
    liftA3 f (pure l) op (operator op p f)

sym = lexeme' bspc . char
kwd = lexeme' bspc . string

name = lexeme' bspc do
  try $ notReserved $ liftA2 (:) lower (many alphaNum)

typeName = lexeme' bspc do
  liftA2 (:) upper (many alphaNum)

termName =
  name

opName = "operator" ?< lexeme' bspc do
  some $ satisfy \c ->
    (C.isPunctuation c || C.isSymbol c) &&
    c /= '(' &&
    c /= ')'

reserved = S.fromList
  [ "let"
  , "in"
  ]

notReserved p = do
  n <- p
  guard $ S.notMember n reserved
  pure n
