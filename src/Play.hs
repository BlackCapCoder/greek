module Play where


import AST
import Data.Char as Chr
import Data.List.NonEmpty qualified as NE
import Numeric (readInt)
import Parser



parser :: Parser Program
parser =
  many sc *> many tld <* eof


tld :: Parser TLD
tld = asum
  [ decl
  ]


-----

reference :: Parser Expr
reference = do
  Reference <$> name

binary :: Parser Integer
binary = do
  string "0b"
  bits <- some $ oneOf "01"
  pure $ fst $ head $ readInt 2 (const True) (subtract 48 . ord) bits

intLit :: Parser Expr
intLit = token $
  IntLit <$> do binary <|> natural

charLit :: Parser Expr
charLit = token do
  char '\''
  CharLit <$> characterChar

stringLit :: Parser Expr
stringLit = do
  StringLit <$> stringLiteral

expr :: Parser Expr
expr = asum
  [ intLit
  , charLit
  , stringLit
  , reference
  ]

-----

kindDecl n = do
  ty <- typeName
  semi
  pure $ KindDecl n ty


decl :: Parser TLD
decl = do

  (k, n) <- someName

  binds <- many name

  chr <-
    token $ oneOf ":="

  case chr of

    ':' -> do
      guard $ null binds

      case k of
        TYPE -> kindDecl n
        TERM -> TypeDecl n <$> tyExprSome <* semi

    '=' -> do

      case k of

        TERM -> do
          e <- expr
          semi
          pure $ TermDecl n binds e

        TYPE -> do
          DataDecl <$> adtDecl n binds


------

dataCase :: Parser DataCase
dataCase = token do
  liftA3 DataCase context typeName (many tyExprOne)

adtDecl :: Name -> [Name] -> Parser ADT
adtDecl n vs = token do
  cs <- sepBy dataCase (symbolic '|')
  semi
  pure $ ADT n vs cs

