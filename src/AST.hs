module AST where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE


type Program = [TLD]


type Name    = String
type VarBind = String


data Kind = TYPE | TERM
  deriving (Eq, Ord, Show, Enum)


data TLD where

  TermDecl :: Name -> [VarBind] -> Expr -> TLD
  TypeDecl :: Name -> TyExpr -> TLD
  KindDecl :: Name -> Name   -> TLD

  DataDecl :: ADT -> TLD

  deriving Show


data Expr where

  Reference :: Name    -> Expr
  IntLit    :: Integer -> Expr
  CharLit   :: Char    -> Expr
  StringLit :: String  -> Expr

  deriving Show




----

data QuantifierKind
   = Universal
   | Existential
   deriving (Eq, Ord, Enum)

data Quantifier
   = Quantifier QuantifierKind [Name]
   deriving (Eq, Ord)


instance Show QuantifierKind where
  show = \case
    Universal   -> "∀"
    Existential -> "∃"

instance Show Quantifier where
  show = \case
    Quantifier k vs -> unwords (show k : vs) <> "."

----

-- quantifiers + constraints

newtype Context = Context [Quantifier]
  deriving (Show)

----

data TyExpr where

  -- A lowercase type variable like `a`
  TyExVar :: Name -> TyExpr

  -- An uppercase type reference like `Int`
  TyExRef :: Name -> TyExpr

  -- A string of types like `f (g Int) b`
  TyExGroup :: NonEmpty TyExpr -> TyExpr

  -- A quantifier like `∀ a b. ...`
  TyExQuant :: Quantifier -> TyExpr -> TyExpr


instance Show TyExpr where

  show = go False where

    go pars = \case

      TyExVar n -> n
      TyExRef n -> n

      TyExQuant q e ->
        parens $ show q <> " " <> go False e

      TyExGroup es ->
        parens $ unwords $ map (go True) $ NE.toList es

      where
        parens s
          | pars = '(' : s ++ ")"
          | let  = s

-----


data DataCase where

  DataCase
    :: Context
    -> Name
    -> [TyExpr]
    -> DataCase


data ADT where

  ADT :: Name       -- the name of this type
      -> [Name]     -- type variables
      -> [DataCase] -- sum of products
      -> ADT


deriving instance Show DataCase
deriving instance Show ADT

