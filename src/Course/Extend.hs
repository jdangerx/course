{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Extend where

import Course.Core
import Course.Id
import Course.List
import Course.Optional
import Course.Functor

-- | All instances of the `Extend` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. (f <<=) . (g <<=) ≅ (<<=) (f . (g <<=) <<=)
class Functor f => Extend f where
  -- Pronounced, extend.
  (<<=) ::
    (f a -> b)
    -> f a
    -> f b

infixr 1 <<=

-- | Implement the @Extend@ instance for @Id@.
--
-- >>> id <<= Id 7
-- Id (Id 7)
instance Extend Id where
  (<<=) ::
    (Id a -> b)
    -> Id a
    -> Id b
  f <<= a = Id (f a)

-- | Implement the @Extend@ instance for @List@.
--
-- >>> length <<= ('a' :. 'b' :. 'c' :. Nil)
-- [3,2,1]
--
-- >>> id <<= (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)
-- [[[4,5,6],[1,2,3]],[[4,5,6]]]
instance Extend List where
  (<<=) ::
    (List a -> b)
    -> List a
    -> List b
  _ <<= Nil = Nil
  f <<= ls@(_:.t) = f ls :. (f <<= t)

-- Reading the doctests, it looks like you run the first function on
-- successive tails of the list and then concatenate them.  At first,
-- I tried to work with the whole list without deconstructing it, but
-- I was trying to make it work from too high a level of abstraction
-- and had to move down a level.

-- | Implement the @Extend@ instance for @Optional@.
--
-- >>> id <<= (Full 7)
-- Full (Full 7)
--
-- >>> id <<= Empty
-- Empty
instance Extend Optional where
  (<<=) ::
    (Optional a -> b)
    -> Optional a
    -> Optional b
  -- f <<= a = case a of
    -- Empty -> Empty
    -- _ ->  Full $ f a
  -- his solution:
  f <<= a = f . Full <$> a

-- Instead of using `case` and going low level, you can just stay at
-- the <$> level.  His solution tacks on an extra `Full` to be
-- stripped off by `f`, whereas mine pulls the value out of the
-- `Optional` and then puts the `Full` or `Empty` back on based on the
-- value.  A little clunkier by comparison.  This time I started too
-- low - which is ok, since I was able to do it, but it was clunkier
-- and if the problem was harder maybe I wouldn't have been
-- successful.

-- | Duplicate the functor using extension.
--
-- >>> cojoin (Id 7)
-- Id (Id 7)
--
-- >>> cojoin (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> cojoin (Full 7)
-- Full (Full 7)
--
-- >>> cojoin Empty
-- Empty
cojoin ::
  Extend f =>
  f a
  -> f (f a)
cojoin f = id <<= f

-- `(<<=) :: (f a -> b) -> f a -> f b` : so if we have `b ~= f a` then
-- we get `::(f a -> f a) -> f a -> f (f a)`, now we just toss in an
-- `id` and be done with it.
