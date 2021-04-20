module Parser
  ( module Parser
  , module Parser.Parser
  )
  where

import Parser.Parser
import AST
import qualified Data.List.NonEmpty as NE
import Control.Monad.Reader
import Text.Parser.LookAhead


---------
-- Literals

lit :: Parser Lit
lit = asum
  [ charLit
  , strLit
  , intLit
  , letLit
  , refLit
  , lamLit
  ]

charLit :: Parser Lit
charLit = tok "character" do
  char '\''
  CharLit <$> characterChar

strLit :: Parser Lit
strLit = tok "string" do
  char '"'
  (StringLit <$> many characterChar) <* char '"'

intLit :: Parser Lit
intLit = tok "integer" $ IntLit <$> asum
  [ char '0' *> hexadecimal
  , octal
  , decimal
  ]

refLit :: Parser Lit
refLit = tok "reference" do
  RefLit <$> name

lamLit :: Parser Lit
lamLit = tok "lambda" do
  LamLit <$> lam

letLit :: Parser Lit
letLit = tok "let block" do
  string "let"
  some spc
  let sep = void (lookAhead (kwd "in")) <|> end
  ts <- lexeme' bspc $
    scoped $ manyTill (tld <* sep) (kwd "in")
  LetLit ts <$> lit

----

lam :: Parser (Lam Lit)
lam = tok "lambda" do
  lam' (sym 'λ') (sym '.') lit lam Abs

lam' pre sep lit lam f = lexeme' bspc do
  pre
  vs <- NE.fromList <$> some (lexeme' bspc name)
  sep
  f vs <$> appLam lam lit

appLam lam lit =
  foldl1 App <$> some do
    asum
      [ lam
      , lexeme' bspc $ sym '(' *> appLam lam lit <* sym ')'
      , Var <$> lit
      ]

--------
-- typelit

quantifier :: Parser QuantifierKind
quantifier = tok "quantifier" do
  asum
    [ Universal   <$ char '∀'
    , Existential <$ char '∃'
    ]

tyLit :: Parser TyLit
tyLit =
  tyOp' $
  foldl1 TyApp <$> some do
    asum
      [ lexeme' bspc $ sym '(' *> (tyOp <|> tyLit) <* sym ')'
      , tyAbs
      , tyRef
      , tyVar
      ]
  where
    tyOp' p =
      operator tyOp p \l o r -> (o `TyApp` l) `TyApp` r

tyRef :: Parser TyLit
tyRef = tok "tyref" do
  TyRef <$> typeName

tyVar :: Parser TyLit
tyVar = tok "tyvar" do
  TyVar <$> name

tyOp :: Parser TyLit
tyOp = tok "tyop" do
  TyOp <$> opName

tyAbs :: Parser TyLit
tyAbs = tok "tyabs" do
  q  <- quantifier
  vs <- NE.fromList <$> some (lexeme' bspc name)
  sym '.'
  TyAbs q vs <$> tyLit

--------
-- top level defns

tld :: Parser TLD
tld = asum
  [ subscope fixityDecl
  , try $ subscope typeDecl
  , subscope termDecl
  , subscope classDecl
  ]

termDecl :: Parser TLD
termDecl = tok "term declaration" do
  lam' (pure ()) (sym '=') lit lam
    \(n NE.:| vs) -> TermDecl n vs

classDecl = tok "class declaration" do
  ClassDecl <$> _class

_class :: Parser Class
_class = do
  char 'Γ' *> some hspc
  (n, vs) <- scoped $ liftA2 (,) typeName tyLit
  ts <- many (try lineCont *> tld)
  pure $ Class n vs ts

typeDecl :: Parser TLD
typeDecl = tok "type declaration" do
  n <- termName
  sym ':'
  TypeDecl n <$> tyLit

fixityDecl :: Parser TLD
fixityDecl = tok "fixity declaration" do
  string "infix"
  dir <- flip map (optional (char 'l' <|> char 'r')) \case
    Just 'l' -> L
    Just 'r' -> R
    Nothing -> LR
  some bspc
  pri <- lexeme' bspc decimal
  ns  <- NE.fromList <$> some opName
  pure $ FixityDecl (Fixity dir (fi pri)) ns

