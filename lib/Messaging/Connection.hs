module Messaging.Connection
  ( createMessagingChannel
  ) where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.AMQP (Channel, openChannel, openConnection')
import Network.Socket

import LensesConfig
import Model.Config.AppConfig

createMessagingChannel :: AppConfig -> IO (Maybe Channel)
createMessagingChannel dswConfig =
  let appMessagingConfig = dswConfig ^. messaging
      mcEnabled = appMessagingConfig ^. enabled
      mcHost = fromMaybe "" $ appMessagingConfig ^. host
      mcPort = fromInteger (fromMaybe 0 $ dswConfig ^. messaging ^. port) :: PortNumber
      mcUsername = T.pack . fromMaybe "" $ appMessagingConfig ^. username
      mcPassword = T.pack . fromMaybe "" $ appMessagingConfig ^. password
  in if mcEnabled
       then do
         channel <- (openConnection' mcHost mcPort "/" mcUsername mcPassword) >>= openChannel
         return (Just channel)
       else return Nothing
