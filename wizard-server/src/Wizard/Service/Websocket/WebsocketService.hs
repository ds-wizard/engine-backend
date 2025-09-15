module Wizard.Service.Websocket.WebsocketService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Shared.Common.Util.Number
import Wizard.Model.Context.AppContext
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.User.UserMapper

createRecord :: U.UUID -> Connection -> String -> WebsocketPerm -> [U.UUID] -> AppContextM WebsocketRecord
createRecord connectionUuid connection entityId permission userGroupUuids = do
  mCurrentUser <- asks currentUser
  avatarNumber <- liftIO $ generateInt 20
  colorNumber <- liftIO $ generateInt 12
  let user = toOnlineUserInfo mCurrentUser avatarNumber colorNumber userGroupUuids
  return $ WebsocketRecord connectionUuid connection entityId permission user

filterEditors :: [WebsocketRecord] -> [WebsocketRecord]
filterEditors records =
  let isEditor record = record.entityPerm == EditorWebsocketPerm
   in filter isEditor records

filterCommenters :: [WebsocketRecord] -> [WebsocketRecord]
filterCommenters records =
  let isCommenter record = record.entityPerm == EditorWebsocketPerm || record.entityPerm == CommenterWebsocketPerm
   in filter isCommenter records
