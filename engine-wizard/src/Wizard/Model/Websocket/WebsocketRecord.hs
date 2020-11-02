module Wizard.Model.Websocket.WebsocketRecord where

import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Wizard.Model.User.OnlineUserInfo

data WebsocketRecord =
  WebsocketRecord
    { _websocketRecordConnectionUuid :: U.UUID
    , _websocketRecordConnection :: Connection
    , _websocketRecordEntityId :: String
    , _websocketRecordEntityPerm :: WebsocketPerm
    , _websocketRecordUser :: OnlineUserInfo
    }

data WebsocketPerm
  = NoWebsocketPerm
  | ViewerWebsocketPerm
  | EditorWebsocketPerm
  deriving (Show, Eq)
