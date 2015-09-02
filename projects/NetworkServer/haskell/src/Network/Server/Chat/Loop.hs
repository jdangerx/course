module Network.Server.Chat.Loop where

import Prelude hiding (mapM_, catch)
import Network(PortID(..), sClose, withSocketsDo, listenOn)
import System.IO(BufferMode(..))
import Data.IORef(IORef, newIORef, readIORef)
import Data.Foldable(Foldable, mapM_)
import Control.Applicative(Applicative, pure)
import Control.Concurrent(forkIO)
import Control.Exception(finally, try, catch, Exception)
import Control.Monad(forever)
import Control.Monad.Trans(MonadTrans(..), MonadIO(..))

import Network.Server.Common.Accept
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Env
import Network.Server.Common.Ref
import Data.Set(Set)
import qualified Data.Set as S

data Loop v f a =
  Loop (Env v -> f a)
-- `Loop` is a function that reads something from an `Env` and
-- produces some value wrapped in a monad.  And an `Env v` is just a
-- container for a client, a list of all clients, and some value of
-- type `v`.

type IOLoop v a =
  Loop v IO a
-- IOLoop is a Loop that takes an `Env` and returns an `IO a`.

type IORefLoop v a =
  IOLoop (IORef v) a
-- `IORefLoop` is a `Loop (IORef v) IO a`. Basically the env value is now
-- a mutable variable in the IO monad.

instance Functor f => Functor (Loop v f) where
  fmap f (Loop k) =
    Loop (fmap f . k)
  -- `f <$> (Loop k) :: Loop (Env v -> f b)`

  -- If we apply `k` to an Env we get `a` wrapped in a monad. So we
  -- need to `fmap f` into it.

instance Applicative f => Applicative (Loop v f) where
  pure = Loop . pure . pure
  Loop k <*> Loop l = Loop (\env -> k env <*> l env)

  -- `pure :: a -> Loop v f`

  -- So we need to make a Loop (Env v -> f a); `Loop (pure _)` wants
  -- `_ :: f a` so we need another `pure a` in there.

  -- `k :: Env v -> f (a -> b)`

  -- `l :: Env v -> f a`

  -- So `k env <*> l env :: f b`, and we can stick that in a `Loop` to
  -- get what we want.  Basically, we take an `env`, run `l` and `k`
  -- on it, then jam those results together so we get an `f b`.

instance Monad f => Monad (Loop v f) where
  return =
    Loop . return . return
  Loop k >>= f =
    Loop (\v -> k v >>= \a ->
      let Loop l = f a
      in l v)
  -- `return` is `pure`

  -- Calling the type `f` `m` for now because of confusion. `f` is the
  -- function, `m` is the monad. `(>>=)`: we need a `Loop v m b` where
  -- `f :: a -> Loop v m b`

  -- So we run `k` on `env`, producing an `m a`, bind that into a
  -- function that takes `a`, gets a function `l :: Env v -> m b` out
  -- of it by deconstructing `f a`.  We need to get a `m b` out of the
  -- `\v -> ...` function, we can run `l` on `v` to get that.

  -- So in plein English we take an environment, run the first loop on
  -- it, run a function on the result of the first loop to get another
  -- loop, and run that on the environment to get a new type of value
  -- out wrapped in the same monad.

instance MonadTrans (Loop v) where
  lift =
    Loop . const
  -- `lift` takes a monadic value and wraps it in a `Loop v`.

instance MonadIO f => MonadIO (Loop v f) where
  liftIO =
    lift . liftIO

etry ::
  Exception e =>
  (Env v -> IO a)
  -> IOLoop v (Either e a)
etry k =
  Loop $ try . k

server ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> IOLoop v () -- per-client
  -> IO a
server i r (Loop f) =
  let hand s w c = forever $
                     do q <- accept' s
                        lSetBuffering q NoBuffering
                        _ <- atomicModifyIORef_ c (S.insert (refL `getL` q))
                        x <- r w
                        forkIO (f (Env q c x))
  in withSocketsDo $ do
       s <- listenOn (PortNumber 6060)
       w <- i
       c <- newIORef S.empty
       hand s w c `finally` sClose s

allClients :: IOLoop v (Set Ref)
allClients = Loop $ \env -> readIORef (clientsL `getL` env)

perClient ::
  IOLoop v x -- client accepted (post)
  -> (String -> IOLoop v a) -- read line from client
  -> IOLoop v ()
perClient (Loop q) f =
  Loop (\env ->
         forever $ do
           input <- lGetLine (acceptL `getL` env)
           let Loop process = f input
           process env
       )


loop ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> IOLoop v x -- client accepted (post)
  -> (String -> IOLoop v w) -- read line from client
  -> IO a
loop i r q f =
  server i r (perClient q f)

iorefServer ::
  v -- server initialise
  -> IORefLoop v () -- per-client
  -> IO a
iorefServer x =
  server (newIORef x) return

iorefLoop ::
  v -- server initialise
  -> IORefLoop v x -- client accepted (post)
  -> (String -> IORefLoop v w) -- read line from client
  -> IO a
iorefLoop x q f =
  iorefServer x (perClient q f)
pPutStrLn ::
  String
  -> IOLoop v ()
pPutStrLn s =
  Loop (`lPutStrLn` s)

(!) ::
  Foldable t =>
  IOLoop v (t Ref)
  -> String
  -> IOLoop v ()
clients ! msg =
 clients >>= purgeClients (\y -> liftIO (lPutStrLn y msg))

infixl 2 !

purgeClients ::
  Foldable t =>
  (Ref -> IOLoop v ())
  -> t Ref
  -> IOLoop v ()
purgeClients a =
  mapM_ (\y ->
    ecatch (a y)
      (\x -> do _ <- modifyClients (S.delete y)
                xprint x)
        )

readEnv ::
  Applicative f =>
  Loop v f (Env v)
readEnv =
  Loop $ pure

readEnvval ::
  Applicative f =>
  Loop v f v
readEnvval =
  fmap (envvalL `getL`) readEnv

readIOEnvval ::
  IORefLoop a a
readIOEnvval =
  Loop $ \env ->
    readIORef (envvalL `getL` env)

allClientsButThis ::
  IOLoop v (Set Ref)
allClientsButThis =
  Loop $ \env ->
    fmap (S.delete ((acceptL .@ refL) `getL` env)) (readIORef (clientsL `getL` env))

-- Control.Monad.CatchIO
ecatch ::
  Exception e =>
  IOLoop v a
  -> (e -> IOLoop v a)
  -> IOLoop v a
ecatch (Loop k) f =
  Loop $ \env -> k env `catch` (\e -> let Loop l = f e in l env)

modifyClients ::
  (Set Ref -> Set Ref)
  -> IOLoop v (Set Ref)
modifyClients f =
  Loop $ \env ->
    atomicModifyIORef_ (clientsL `getL` env) f
