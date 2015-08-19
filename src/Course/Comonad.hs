{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Comonad
(
  Comonad(..)
) where

import Course.Core
import Course.Extend
import Course.Id

-- | All instances of the `Comonad` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. copure <<= x ≅ x`
--
-- * The law of right identity
--   `∀f. copure . (f <<=) == f
class Extend f => Comonad f where
  copure ::
    f a
    -> a

-- | Implement the @Comonad@ instance for @Id@.
--
-- >>> copure (Id 7)
-- 7
instance Comonad Id where
  copure ::
    Id a
    -> a
  copure (Id a) = a
    -- error "todo: Course.Comonad copure#instance Id"

-- | Witness that all things with (<<=) and copure also have (<$>).
--
-- >>> (+10) <$> Id 7
-- Id 17
(<$>) ::
  Comonad f =>
  (a -> b)
  -> f a
  -> f b
f <$> a = (f . copure) <<= a

-- You can think of `<$>` as sort of "extending" the functor on the
-- right side over the function on the left side.

-- Extend `a :: f a` into whatever is on the left side - the left side
-- now wants `f a -> b` - so if you attach a `copure` to that you get
-- `a -> b`, which is the type of `f`.
