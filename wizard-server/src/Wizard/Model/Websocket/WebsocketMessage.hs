module Wizard.Model.Websocket.WebsocketMessage where

import qualified Data.UUID as U
import Network.WebSockets (Connection)

data WebsocketMessage content = WebsocketMessage
  { connectionUuid :: U.UUID
  , connection :: Connection
  , entityId :: String
  , content :: content
  }
