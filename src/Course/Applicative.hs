module Course.Applicative(
 Applicative(..)
) where

import Course.Core
import Course.Apply
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

class Apply f => Applicative f where
  pure ::
    a -> f a

-- todo fix Exercise

-- Exercise 6
-- Relative Difficulty: 3
--
-- | Witness that all things with bind and return also have fmap.
--
-- >>> (+1) <$> (Id 2)
-- Id 3
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
(<$>) ::
  Applicative f =>
  (a -> b)
  -> f a
  -> f b
(<$>) =
  error "todo"

-- Exercise 7
--
-- | Insert into the Id monad.
--
-- prop> return x == Id x
instance Applicative Id where
  pure =
    error "todo"

-- Exercise 8
--
-- | Insert into a List.
--
-- prop> return x == x :. Nil
instance Applicative List where
  pure =
    error "todo"

-- Exercise 9
--
-- | Insert into an Optional.
--
-- prop> return x == Full x
instance Applicative Optional where
  pure =
    error "todo"

-- Exercise 10
-- Relative Difficulty: 3
--
-- | Insert into a constant function.
--
-- prop> return x y == x
instance Applicative ((->) t) where
  pure =
    error "todo"

-- Exercise 16
--
-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (Id 7 :. Id 8 :. Id 9 :. Nil)
-- Id [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence ::
  Applicative f =>
  List (f a)
  -> f (List a)
sequence =
  error "todo"

-- Exercise 18
-- Relative Difficulty: 4
--
-- | Replicate an effect a given number of times.
--
-- >>> replicate 4 (Id "hi")
-- Id ["hi","hi","hi","hi"]
--
-- >>> replicate 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicate 4 Empty
-- Empty
--
-- >>> replicate 4 (*2) 5
-- [10,10,10,10]
replicate ::
  Applicative f =>
  Int
  -> f a
  -> f (List a)
replicate =
  error "todo"

-- Exercise 19
-- Relative Difficulty: 9
--
-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (Id . even) (4 :. 5 :. 6 :. Nil)
-- Id [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
filtering ::
  Applicative f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filtering =
  error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return

instance Applicative [] where
  pure =
    P.return

instance Applicative P.Maybe where
  pure =
    P.return