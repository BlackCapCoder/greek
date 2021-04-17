module TwoThree where

import Prelude hiding (lefts, rights)
import Data.Bool (bool)
import Data.Bifunctor (bimap)
import Tape
import Trit



type Dir    = Bit  -- L | R
type State  = Bit  -- A | B
type Symbol = Trit -- X | Y | Z

pattern L = O :: Dir
pattern R = I :: Dir

pattern A = O :: State
pattern B = I :: State


ppState :: State -> String
ppState = \case
  A -> "A"
  B -> "B"

----

-- 2,3 machine
--
-- https://en.wikipedia.org/wiki/Wolfram%27s_2-state_3-symbol_Turing_machine

data Machine = M
  { state :: State
  , tape  :: Tape Symbol
  }


-- 2,3 is known to be universal under this transition table:
--
-- https://www.wolframscience.com/prizes/tm23/
-- https://www.complex-systems.com/abstracts/v29_i01_a01/

system0 = \case
  (A, X) -> (B, Y, R)
  (A, Y) -> (A, Z, L)
  (A, Z) -> (A, Y, L)

  (B, X) -> (A, Z, L)
  (B, Y) -> (B, Z, R)
  (B, Z) -> (A, X, R)


-- Get the current state and active tape symbol
now :: Machine -> (State, Symbol)
now M {..} =
  (state, copoint tape)


-- Overwrite state and symbol, then move the tape head
trans :: (State, Symbol, Dir) -> Machine -> Machine
trans (st, sy, dir) (M _ (Tape l _ r))
  = M st
  . bool moveL moveR dir
  $ Tape l sy r


-- Transition once using system 0
step :: Machine -> Machine
step =
  now >>= trans . system0


-- Transition forever, scanning the active tape symbol
-- trace :: Machine -> Stream Symbol
trace =
  liftA2 (:-) now (trace . step)

-- Pretty print.
pp = trace
 >>> toList
 >>> take 40
 >>> map do \(st, sy) -> ppState st <> show sy
 >>> unlines

----

m1 :: Machine
m1 = M A (point X)

