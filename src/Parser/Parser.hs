module Parser.Parser
  ( module Parser.Parser
  , module Text.Trifecta
  )
  where

import Text.Trifecta hiding (Parser)
import Text.Trifecta qualified as P

import Data.Char as Chr


newtype Parser a
      = Parser { toParser :: P.Parser a }

  deriving newtype
    ( Functor, Applicative, Monad
    , Semigroup, Monoid
    , Alternative, MonadPlus, MonadFail
    , Parsing, CharParsing
    )

instance TokenParsing Parser where

  someSpace = skipSome sc



lineComment :: Parser ()
lineComment = do
  string "--"
  void $ manyTill anyChar newline

blockComment :: Parser ()
blockComment = do
  string "{-"
  void $ manyTill anyChar $ string "-}"

sc :: Parser ()
sc = asum
  [ void $ satisfy isSpace
  , void newline
  , lineComment
  , blockComment
  ]


