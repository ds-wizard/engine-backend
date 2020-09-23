module Wizard.Messaging.Connection
  ( createMessagingChannel
  ) where

import Control.Concurrent.MVar (MVar, putMVar)
import qualified Control.Exception as E
import Control.Lens ((^.))
import qualified Data.Text as T
import Network.AMQP (Channel, addChannelExceptionHandler, openChannel, openConnection')
import Network.Socket

import LensesConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Util.Logger

createMessagingChannel :: ServerConfig -> MVar () -> IO (Maybe Channel)
createMessagingChannel serverConfig shutdownFlag =
  let appMessagingConfig = serverConfig ^. messaging
      mcEnabled = appMessagingConfig ^. enabled
      mcHost = appMessagingConfig ^. host
      mcPort = fromInteger (serverConfig ^. (messaging . port)) :: PortNumber
      mcUsername = T.pack $ appMessagingConfig ^. username
      mcPassword = T.pack $ appMessagingConfig ^. password
      mcVhost = T.pack $ appMessagingConfig ^. vhost
      logMsg msg = runLogging (serverConfig ^. logging . level) (logInfo _CMP_MESSAGING msg)
   in if mcEnabled
        then do
          channel <- openConnection' mcHost mcPort mcVhost mcUsername mcPassword >>= openChannel
          addChannelExceptionHandler channel (handleChannelClose logMsg shutdownFlag)
          return (Just channel)
        else return Nothing

handleChannelClose :: (String -> IO ()) -> MVar () -> E.SomeException -> IO ()
handleChannelClose logMsg shutdownFlag _ = do
  logMsg "message broker disconnect us -> shutting down the app"
  putMVar shutdownFlag ()
