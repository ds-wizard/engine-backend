module Messaging.Connection where

import Control.Lens ((^.))
import qualified Data.Text as T
import Network
import Network.AMQP (Channel, openChannel, openConnection')

import LensesConfig
import Model.Config.AppConfig

createMessagingChannel :: AppConfig -> IO Channel
createMessagingChannel dswConfig =
  let appMessagingConfig = dswConfig ^. messagingConfig
      mcHost = appMessagingConfig ^. host
      mcPort = fromInteger (dswConfig ^. messagingConfig ^. port) :: PortNumber
      mcUsername = T.pack $ appMessagingConfig ^. username
      mcPassword = T.pack $ appMessagingConfig ^. password
  in (openConnection' mcHost mcPort "/" mcUsername mcPassword) >>= openChannel
