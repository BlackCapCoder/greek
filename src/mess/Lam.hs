module Lam where

import Data.Profunctor
import Data.Profunctor.Types
import Control.Comonad


data Lam a where
  Var :: a -> Lam a
  Abs :: (Profunctor p, p ~ (->)) => (Lam a `p` Lam b) -> Lam (a `p` b)
  App :: Lam (a -> b) -> Lam a -> Lam b


-- pattern (:$) f a = App f a; infixr 1 :$

-- s ∿ (k $$ s ∿ p s ∿ k) ∿ s

(∿) = App; infixl 8 ∿
($$) = App; infixr 1 $$

bc -. ab = (b $$ bc) $$ ab



i :: Lam (a -> a)
i = Abs id

k :: Lam (a -> b -> a)
k = Abs $ Abs . const

s :: Lam ((a -> c -> b) -> (a -> c) -> a -> b)
s = Abs \f -> Abs \g -> Abs \a -> App f a $$ App g a


b :: Lam ((b -> c) -> (a -> b) -> a -> c)
b = (s $$ k $$ s) $$ k
-- s (k s) k

w :: Lam ((c -> c -> b) -> c -> b)
w = (s $$ s) $$ s $$ k

c :: Lam ((a -> b -> c) -> (b -> a -> c))
c = s ∿( s ∿( k ∿( s ∿(k∿s) ∿k)) ∿s) ∿ (k∿k)

-- C = S (S (K (S (K S) K)) S) (K K)


runLam :: ∀ x. Lam x -> x
runLam = \case
  Var a   -> a
  Abs p   -> dimap Var runLam p
  App f a -> runLam f (runLam a)


instance Pointed Lam where
  point = Var

instance Copointed Lam where
  copoint = runLam

instance Functor Lam where
  fmap = App . Var

instance Apply Lam where
  (<.>) = App

instance Bind Lam where
  join = copoint

-- instance Comonad Lam where
--
--   extract = \case
--     Var a   -> a
--     Abs p   -> dimap Var extract p
--     App f a -> extract f (extract a)
--
--   extend f =
--     App (Abs (Var . f))
--
--
--   duplicate :: Lam a -> Lam (Lam a)
--   duplicate =
--     App (Abs Var)

