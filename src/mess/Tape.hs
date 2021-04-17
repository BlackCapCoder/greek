module Tape where

import Prelude hiding (lefts, rights)


infixr 5 :-

data Stream a
   = a :- Stream a
   deriving
    ( Show, Functor, Foldable
    )

data Tape a
   = Tape (Stream a) a (Stream a)
   deriving
     ( Show, Functor
     )


instance Copointed Stream where
  copoint (a :- _) = a

instance Pointed   Stream where
  point a = a :- point a


instance Copointed Tape where
  copoint (Tape _ a _) = a

instance Pointed   Tape where
  point a = Tape (point a) a (point a)


moveL (Tape ls a (r:-rs)) = Tape (a:-ls) r rs
moveR (Tape (l:-ls) a rs) = Tape ls l (a:-rs)

lefts  t = t :- lefts  (moveL t)
rights t = t :- rights (moveR t)

scycle xs = go xs where
  go (a:as) = a :- go as
  go []     = go xs

stail (_ :- xs) = xs

----

-- interleave
instance Semigroup (Stream a) where
  (a:-as) <> (b:-bs) =
    a :- b :- (as <> bs)

instance Semigroup (Tape a) where
  Tape ll l lr <> Tape rl r rr =
    Tape (ll <> rl) l (r :- (<>) lr rr)


instance Applicative Stream where
  pure =
    point

  (f :- fs) <*> (a :- as) =
    f a :- (fs <*> as)

instance Applicative Tape where
  pure =
    point

  Tape fl f fr <*> Tape al a ar =
    Tape (fl <*> al) (f a) (fr <*> ar)


instance Comonad Stream where
  extract =
    copoint

  duplicate s@(_ :- xs) =
    s :- duplicate xs

instance Comonad Tape where
  extract =
    copoint

  duplicate t@(Tape l x r) =
    Tape (stail $ lefts t) t (stail $ rights t)


