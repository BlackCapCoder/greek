module Trit where

import Iso
import Prelude hiding (Unit)


type Bits  = [] Bit
type Trits = [] Trit

----

type Succ = Maybe

pattern S a = Just a  :: Succ x
pattern V   = Nothing :: Succ x

----

type Unit = ()

pattern U = () :: Unit


type Bit = Bool

pattern O = False :: Bit
pattern I = True  :: Bit


data Trit = X | Y | Z
  deriving
    ( Show, Eq, Ord, Enum, Bounded
    )

----

type SUnit = Succ Unit

sunit :: Bit -> SUnit
sunit = \case
  O -> V
  I -> S U

bit :: SUnit -> Bit
bit = \case
  V   -> O
  S U -> I

bitUnit :: Bit <-> SUnit
bitUnit = Iso sunit bit

----

type SBit = Succ Bit

sbit :: Trit -> SBit
sbit = \case
  X -> V
  Y -> S O
  Z -> S I

trit :: SBit -> Trit
trit = \case
  V   -> X
  S O -> Y
  S I -> Z

tritBit :: Trit <-> SBit
tritBit = Iso sbit trit

-----

easy :: Bits
easy =
  join $ replicate 16 [O, I]

-- Random
hard :: Bits
hard =
  [ O, O, I, O, I, I, I, I
  , O, I, O, I, I, O, O, O
  , I, I, O, O, O, O, I, I
  , I, I, I, O, O, O, I, O
  ]

