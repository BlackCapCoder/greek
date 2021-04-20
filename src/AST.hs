module AST where

import Data.List.NonEmpty (NonEmpty)
import Data.List (intercalate)


type Name = String


data Lit where
  RefLit    :: Name    -> Lit
  IntLit    :: Integer -> Lit
  CharLit   :: Char    -> Lit
  StringLit :: String  -> Lit
  LamLit    :: Lam Lit -> Lit
  LetLit    :: [TLD] -> Lit -> Lit

data Lam a where
  Var :: a -> Lam a
  Abs :: NonEmpty Name -> Lam a -> Lam a
  App :: Lam a -> Lam a -> Lam a

data TLD where
  TermDecl   :: Name -> [Name] -> Lam Lit -> TLD
  TypeDecl   :: Name -> TyLit -> TLD  -- type of term
  ClassDecl  :: Class -> TLD
  FixityDecl :: Fixity -> NonEmpty Name -> TLD


data Class where
  Class :: Name -> TyLit -> [TLD] -> Class

data FixDir
   = L | R | LR

data Fixity
   = Fixity FixDir Int


-----

data QuantifierKind
   = Universal
   | Existential
   deriving (Eq, Ord, Enum)

data TyLit where

  -- Uppercase name of type
  TyRef :: Name
        -> TyLit

  -- Operator name
  TyOp :: Name
       -> TyLit

  -- Lowercase name of tyvar
  TyVar :: Name
        -> TyLit

  TyAbs :: QuantifierKind
        -> NonEmpty Name
        -> TyLit
        -> TyLit

  TyApp :: TyLit
        -> TyLit
        -> TyLit

------

instance Show Lit where
  show = \case
    RefLit  n     -> n
    IntLit  i     -> show i
    StringLit str -> show str
    LamLit lam    -> show lam
    CharLit c     -> ['\'', c]
    LetLit  ts a -> unwords ["let", show ts, "in", show a]

instance Show a => Show (Lam a) where
  show = \case
    Var lit -> show lit
    Abs a b -> "λ" <> unwords (show <$> toList a) <> ". " <> show b
    App a b -> unwords [show a, par b]
    where
      par b@(App _ _) = "(" <> show b <> ")"
      par b           = show b

instance Show TLD where
  show = \case
    TermDecl n vs la -> unwords (n:vs) <> " = " <> show la
    TypeDecl n la    -> unwords [n, ":", show la]
    ClassDecl c      -> show c
    FixityDecl f ns  -> show f <> " " <> intercalate ", " (toList ns)

instance Show Class where
  show = \case
    Class n vs ts ->
      unlines do
        unwords ["Γ", n, show vs] :
          map (("  " <>) . show) ts

instance Show QuantifierKind where
  show = \case
    Universal   -> "∀"
    Existential -> "∃"

instance Show TyLit where
  show = go False where

    par a =
      "(" <> a <> ")"

    pargo a =
      par (go False a)

    parif b a | b = par a | let = a

    go p = \case
      TyRef a -> a
      TyVar a -> a
      TyOp  a -> par a
      TyAbs q vs a ->
        parif p $ unwords (show q : toList vs) <> ". " <> go False a

      TyApp (TyApp (TyOp o) l@(TyApp (TyApp (TyOp _) _) _)) r ->
        unwords [pargo l, o, go p r]

      TyApp (TyApp (TyOp o) l@(TyApp (TyOp _) _)) r ->
        unwords [pargo l, o, go p r]

      TyApp (TyApp (TyOp o) l) r ->
        unwords [go p l, o, go p r]

      TyApp a b@(TyApp _ _) ->
        unwords [go p a, pargo b]

      TyApp a b ->
        unwords [go p a, go p b]

instance Show Fixity where
  show (Fixity L  pri) = "infixl " <> show pri
  show (Fixity R  pri) = "infixr " <> show pri
  show (Fixity LR pri) = "infix "  <> show pri

