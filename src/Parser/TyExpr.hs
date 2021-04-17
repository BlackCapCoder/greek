module Parser.TyExpr where


import Parser.Parser
import Parser.Names
import AST

import Data.List.NonEmpty qualified as NE



quantifierKind = token $ asum
  [ Universal   <$ char '∀'
  , Existential <$ char '∃'
  ]

quantifier = do
  q  <- quantifierKind
  vs <- some tyvar

  Quantifier q vs <$ symbolic '.'


-- Attempt quantification
tyExQuant :: Parser TyExpr -> Parser TyExpr
tyExQuant p = do
  optional quantifier >>= \case
    Nothing -> p
    Just  q -> TyExQuant q <$> tyExQuant p

tyExVar :: Parser TyExpr
tyExVar =
  TyExVar <$> tyvar

tyExRef :: Parser TyExpr
tyExRef =
  TyExRef <$> typeName

tyExParens :: Parser TyExpr
tyExParens =
  token $ parens tyExprSome

tyExprOne :: Parser TyExpr
tyExprOne =
  tyExVar <|> tyExRef <|> tyExParens

tyExprSome :: Parser TyExpr
tyExprSome = token $ tyExQuant do
  ~(e:es) <- some tyExprOne

  pure do
  if null es then
    e
  else
    TyExGroup $ NE.fromList (e:es)


---------

context :: Parser Context
context = token do
  Context <$> many quantifier

