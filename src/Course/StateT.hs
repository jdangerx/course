{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<$>) fn (StateT k) = StateT (\s -> first fn <$> k s)
-- runStateT sfa on state s to get an f (a, s). Then <$> into it, but
-- since our function :: a -> b and if we do a plain <$> we need (a,
-- s) -> (b, s) we need to wrap it all in a lambda.


-- | Implement the `Apply` instance for @StateT s f@ given a @Bind f@.
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Bind f => Apply (StateT s f) where
  (<*>) ::
    StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  StateT f <*> StateT a = StateT (\s -> f s >>= (\(g, s') -> first g <$> a s'))
  -- we use >>= to grab the resulting (a -> b, s) out of f s.
  -- Then we need something that takes (a -> b, s) and turns it into type f (b, s)
  -- `first g` grabs the a->b out of (a->b, s)) and makes it (a, s) -> (b, s)
  -- now we need to apply `first g :: (a, s) -> (b, s)` to `something :: f (a, s)`
  -- which is just `<$>`, and `something` is `a s'`

-- | Implement the `Applicative` instance for @StateT s f@ given a @Applicative f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure a = StateT (\s -> pure (a, s))

-- | Implement the `Bind` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
instance Monad f => Bind (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  f =<< (StateT a) = StateT (\s -> a s >>= (\(a', s') -> runStateT (f a') s'))
  -- grab the `(a', s') :: (a, s)` out of `a s` with the `>>=`, then
  -- `(f a')` is a `StateT s f b` so we just run that on `s'` to get
  -- our `something :: f (b, s)`

instance Monad f => Monad (StateT s f) where

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT (Id . f)
-- This is the same as `StateT (\s -> Id (f s))`.


-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT a) = runId . a

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT f) s = snd <$> f s
-- f s makes a Functor (value, state) and so you can `fmap` `snd` into
-- it to get the second value of the pair out.

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' f = runId . execT f

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT f) s = fst <$> f s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' f = runId . evalT f

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Monad f =>
  StateT s f s
getT = StateT (\s -> pure (s, s))
-- need the `pure` to wrap in the monad...

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT seed = StateT (const $ pure ((), seed))

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' ls = eval'
               (
                 filtering
                 (state' . (\a s -> (S.notMember a s, S.insert a s)))
                 -- (state' . lift2 (lift2 (,)) S.notMember S.insert) -- ok this is ridiculous.
                 ls
               )
               S.empty

-- use `eval'` and `state'`; in state' constructor we need a function -
-- we use the composition to go from `\a -> state' (\s -> do something
-- with a and s)` to `state' . (\a s -> do something with a and s)`

-- we can do some `lift2` craziness - we need a fn that takes `a` and
-- `s` and pumps out a pair. If you want to get kind of general with
-- things, we can think of the variant part of what we're doing as
-- "how does the state matter?" and "how do we modify the state?".  In
-- this case that means `S.notMember` and `S.insert`. So we might be
-- able to apply some functions to those two things to get *another*
-- function that does exactly what we want to `a` and `s`, without
-- using a lambda. From here we can do hole driven development to
-- `state' . _ S.notMember S.insert`. We see that the hole has a bunch
-- of `S.Set a[012] ->` signatures, and since we're only gonna be
-- talking about one type of set we can probably think of all the
-- `a`'s as one `a` type.  This looks a bit like a place where we can
-- do `state' . lift2 _ S.notMember S.insert`, and we can keep
-- repeating till we get there.

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF ls = evalT
               (
                 filtering
                    (StateT . (\a s -> if a > 100 then Empty
                                       else Full (S.notMember a s, S.insert a s)))
                 ls
               )
               S.empty

-- hey this time the "accepted answer" is basically the same fanciness
-- as the one I started with!



-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  f <$> a = OptionalT ((f <$>) <$> runOptionalT a)
  -- alt: actually readable:
  -- f <$> a = OptionalT (mapOptional f <$> runOptionalT a)

-- double `<$>` is weird. Came to it thusly:

-- `OptionalT (_ runOptionalT a)` - runOptionalT a will get us `sth::f
-- (Optional a)`, so we need to apply our function within the f. Thus,
-- `<$>`.

-- Then we get `OptionalT (f <$> runOptionalT a)`, but compiler
-- complains that `f` needs to be `::Optional a -> Optional b` instead
-- of `a -> b`.  So we need another `<$>`. In this case `<$>` can be
-- replaced with `mapOptional` which makes it way easier to read and
-- understand.

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Apply f => Apply (OptionalT f) where
  -- OptionalT f <*> OptionalT a = OptionalT ((<*>) <$> f <*> a)
  OptionalT f <*> OptionalT a = OptionalT (lift2 (<*>) f a)

-- This makes sense! You are lifting the `<*>` one more level into the
-- `f` that wraps `Optional`, and then using that as the function that
-- spits out an `f (Optional a)`.

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
instance Applicative f => Applicative (OptionalT f) where
  pure a = OptionalT (pure . pure $ a)
  -- first `pure` makes it an Optional, second `pure` wraps it in f

-- | Implement the `Bind` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Bind (OptionalT f) where
  f =<< a = OptionalT (runOptionalT a >>= (\o -> case o of
                                            Empty -> pure Empty
                                            Full v -> runOptionalT (f v)))

-- if we pull the `Optional a` out of the `OptionalT f a` we can then
-- bind it into a simple conditional


instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  f <$> (Logger l a) = Logger l (f a)

-- | Implement the `Apply` instance for `Logger`.
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Apply (Logger l) where
  (Logger lf f) <*> (Logger la a) = Logger (lf ++ la) (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
instance Applicative (Logger l) where
  pure = Logger Nil

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Bind (Logger l) where
  f =<< (Logger l a) = Logger l id <*> f a
  -- (=<<) =
    -- error "todo: Course.StateT (=<<)#instance (Logger l)"

instance Monad (Logger l) where

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l = Logger (l :. Nil)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG ls =
  runOptionalT (
    evalT
    (filtering
     (StateT . (\a s ->
                 OptionalT (
                   if a > 100
                   then log1 (fromString ("aborting > 100: " P.++ show a)) Empty
                   else (if even a
                         then log1 (fromString ("even number: " P.++ show a))
                         else pure) (Full (S.notMember a s, S.insert a s)))
               ))
     ls)
    S.empty
    )

-- We start with `runOptionalT (evalT ...)` because `Logger Chars
-- (Optional (List a))` is of the form `f Optional a` where `f` is
-- `Logger Chars`.  This is kind of analogous to using `runId (evalT
-- ...` for distinct': basically the runOptionalT pulls the `Logger
-- Chars (Optional (List a))` out of the `OptionalT (Logger Chars)
-- (List a)`

-- `(\a s -> OptionalT _)` is pretty familiar - what goes in that hole
-- is pretty tricky though.

-- We know that the hole contains a `Logger Char (Optional a)` - so
-- the various different Logging outcomes are made from different
-- `Logger`s, differentiated here.

-- So we have a nested if/then structure.

-- First branch is pretty straightforward. `log1` is a nice shortcut
-- here, and shortcuts are nice since they avoid typos and such.

-- the else has a nested if/then in it, that only makes a `Logger
-- Chars`. Which `Logger Chars` is made is dependent on the evenness
-- of `a`. The value is **after** the whole `(if ... then ... else
-- ...)` block, since the value is gonna be the same regardless.
