{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  f <$> (Compose g) = Compose ((f <$>) <$> g)

-- remember that `f (g b)` just means that `b` is wrapped in two
-- Functors!  Here we are saying that a Functor of a Functor is still
-- a Functor, i.e. you can still `<$>` over it.  In this case you just
-- `<$>` twice.

instance (Apply f, Apply g) =>
  Apply (Compose f g) where
-- Implement the (<*>) function for an Apply instance for Compose
  (Compose fn) <*> (Compose gn) = Compose (((<*>) <$> fn) <*> gn)

-- gn is a value wrapped in two applicatives. fn is a function wrapped
-- in two applicatives.

-- we need to apply that function to the value inside the applicatives.

-- what does `(<*>) <$> fn` mean? that means that there is something
-- that `<*>` can act on (a function in an applicative) inside a
-- functor - so if we want to appy that function we need to first
-- apply `(<*>)` to `fn` to get the effect of `innerFunction
-- <*>`. Then we double `<*>` since there's two layers of applicatives
-- to drill down.

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure a = Compose (pure (pure a))

-- You can keep `pure`ing over and over and over!
-- remember to *use the typeclasses!!*

instance (Bind f, Bind g) =>
  Bind (Compose f g) where
-- Implement the (=<<) function for a Bind instance for Compose
  f =<< (Compose m) = error "Impossible! That wall is over 50 meters tall!!"
    
  -- this one ain't possible. process of elimination, plus if any of
  -- the others were not possible this wouldn't be either.
