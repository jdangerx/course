module Network.Server.Chat.Chat where

import Network.Server.Common.Line
import Network.Server.Common.Env
import Network.Server.Common.Accept
import Network.Server.Common.Lens
import Network.Server.Chat.Loop
import Data.Maybe(fromMaybe)
import Data.Foldable(msum)
import Data.IORef(atomicModifyIORef)
import Control.Applicative((<$), (<$>))
import Control.Monad.Trans(MonadIO(..))

type Chat a =
  IORefLoop Integer a
  -- A `Chat a` is a loop whose env value is an `IORef Integer` and
  -- whose return value is an `IO a`

data ChatCommand =
  Chat String
  | Incr
  | Unknown String
  deriving (Eq, Show)

incr ::
  Chat Integer
incr =
  do e <- readEnvval
     liftIO $ atomicModifyIORef e (\n -> (n + 1, n + 1))

chat ::
  IO a
chat =
  iorefLoop 0 (readIOEnvval >>= pPutStrLn . show) (process . chatCommand)

-- |
--
-- >>> chatCommand "CHAT hi"
-- Chat "hi"
--
-- >>> chatCommand "Chat bye"
-- Chat "bye"
--
-- >>> chatCommand "INCR"
-- Incr
--
-- >>> chatCommand "Nothing"
-- UNKNOWN "Nothing"
chatCommand ::
  String
  -> ChatCommand
chatCommand z =
  Unknown z `fromMaybe` msum [
                               Chat <$> trimPrefixThen "CHAT" z
                             , Incr <$ trimPrefixThen "INCR" z
                             ]

cleanupHostName :: String -> String
cleanupHostName = reverse . takeWhile (/= ':') . reverse

process ::
  ChatCommand
  -> Chat ()
process Incr = do
  incr
  counter <- readIOEnvval
  hostname <- readEnv >>= (\env -> Loop . pure . pure $ hostNameL `getL` (acceptL `getL` env))
  allClients ! cleanupHostName hostname ++ " > the count is " ++ show counter
process (Chat msg) = do
  hostname <- readEnv >>= (\env -> Loop . pure . pure $ hostNameL `getL` (acceptL `getL` env))
  allClientsButThis ! (cleanupHostName hostname ++ " > " ++ msg)
process (Unknown cmd) = pPutStrLn ("UNKNOWN " ++ cmd)
