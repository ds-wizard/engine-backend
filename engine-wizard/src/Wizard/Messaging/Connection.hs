module Wizard.Messaging.Connection
  ( createMessagingChannel
  ) where

import Control.Lens ((^.))
import qualified Data.Text as T
import Network.AMQP (Channel, openChannel, openConnection')
import Network.Socket

import LensesConfig
import Wizard.Model.Config.ServerConfig

createMessagingChannel :: ServerConfig -> IO (Maybe Channel)
createMessagingChannel serverConfig =
  let appMessagingConfig = serverConfig ^. messaging
      mcEnabled = appMessagingConfig ^. enabled
      mcHost = appMessagingConfig ^. host
      mcPort = fromInteger (serverConfig ^. (messaging . port)) :: PortNumber
      mcUsername = T.pack $ appMessagingConfig ^. username
      mcPassword = T.pack $ appMessagingConfig ^. password
      mcVhost = T.pack $ appMessagingConfig ^. vhost
   in if mcEnabled
        then do
          channel <- openConnection' mcHost mcPort mcVhost mcUsername mcPassword >>= openChannel
          return (Just channel)
        else return Nothing
