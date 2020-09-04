module Wizard.Model.Websocket.WebsocketMessage where

import qualified Data.UUID as U
import Network.WebSockets (Connection)

data WebsocketMessage content =
  WebsocketMessage
    { _websocketMessageConnectionUuid :: U.UUID
    , _websocketMessageConnection :: Connection
    , _websocketMessageEntityId :: String
    , _websocketMessageContent :: content
    }
