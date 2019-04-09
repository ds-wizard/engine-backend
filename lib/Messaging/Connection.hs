module Messaging.Connection
  ( createMessagingChannel
  ) where

import Control.Lens ((^.))
import qualified Data.Text as T
import Network.AMQP (Channel, openChannel, openConnection')
import Network.Socket

import LensesConfig
import Model.Config.AppConfig

createMessagingChannel :: AppConfig -> IO (Maybe Channel)
createMessagingChannel dswConfig =
  let appMessagingConfig = dswConfig ^. messagingConfig
      mcEnabled = appMessagingConfig ^. enabled
      mcHost = appMessagingConfig ^. host
      mcPort = fromInteger (dswConfig ^. messagingConfig ^. port) :: PortNumber
      mcUsername = T.pack $ appMessagingConfig ^. username
      mcPassword = T.pack $ appMessagingConfig ^. password
  in if mcEnabled
       then do
         channel <- (openConnection' mcHost mcPort "/" mcUsername mcPassword) >>= openChannel
         return (Just channel)
       else return Nothing
