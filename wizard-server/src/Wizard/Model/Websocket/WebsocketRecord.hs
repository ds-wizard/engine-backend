module Wizard.Model.Websocket.WebsocketRecord where

import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Wizard.Model.User.OnlineUserInfo

data WebsocketRecord = WebsocketRecord
  { connectionUuid :: U.UUID
  , connection :: Connection
  , entityId :: String
  , entityPerm :: WebsocketPerm
  , user :: OnlineUserInfo
  }

data WebsocketPerm
  = NoWebsocketPerm
  | ViewerWebsocketPerm
  | CommentatorWebsocketPerm
  | EditorWebsocketPerm
  deriving (Show, Eq)
