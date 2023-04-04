module Wizard.Service.Branch.Collaboration.CollaborationService where

import Control.Monad (when)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Branch.Event.BranchEventDTO
import Wizard.Api.Resource.Websocket.BranchActionJM ()
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Branch.Collaboration.CollaborationAcl
import Wizard.Service.Branch.Collaboration.CollaborationMapper
import Wizard.Service.Cache.BranchWebsocketCache
import Wizard.Service.Websocket.WebsocketService
import Wizard.Util.Websocket

putUserOnline :: U.UUID -> U.UUID -> Connection -> AppContextM ()
putUserOnline branchUuid connectionUuid connection = do
  myself <- createRecord connectionUuid connection (U.toString branchUuid) EditorWebsocketPerm
  checkViewPermission myself
  _ <- findBranchByUuid branchUuid
  addToCache myself
  logWS connectionUuid "New user added to the list"
  setUserList branchUuid connectionUuid

deleteUser :: U.UUID -> U.UUID -> AppContextM ()
deleteUser branchUuid connectionUuid = do
  deleteFromCache connectionUuid
  setUserList branchUuid connectionUuid

setUserList :: U.UUID -> U.UUID -> AppContextM ()
setUserList branchUuid connectionUuid = do
  logWS connectionUuid "Informing other users about user list changes"
  records <- getAllFromCache
  broadcast (U.toString branchUuid) records (toSetUserListMessage records) disconnectUser
  logWS connectionUuid "Informed completed"

logOutOnlineUsersWhenBranchDramaticallyChanged :: U.UUID -> AppContextM ()
logOutOnlineUsersWhenBranchDramaticallyChanged branchUuid = do
  records <- getAllFromCache
  let error = NotExistsError $ _ERROR_SERVICE_QTN_COLLABORATION__FORCE_DISCONNECT (U.toString branchUuid)
  traverse_ (logOut error) records
  where
    logOut :: AppError -> WebsocketRecord -> AppContextM ()
    logOut error record =
      when
        (record.entityId == U.toString branchUuid)
        (sendError record.connectionUuid record.connection record.entityId disconnectUser error)

-- --------------------------------
setContent :: U.UUID -> U.UUID -> BranchEventDTO -> AppContextM ()
setContent branchUuid connectionUuid reqDto =
  case reqDto of
    AddBranchEventDTO' event -> addBranchEvent branchUuid connectionUuid event

addBranchEvent :: U.UUID -> U.UUID -> AddBranchEventDTO -> AppContextM ()
addBranchEvent branchUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  appendBranchEventByUuid branchUuid [reqDto.event]
  records <- getAllFromCache
  broadcast (U.toString branchUuid) records (toAddBranchMessage reqDto) disconnectUser

-- --------------------------------
disconnectUser :: ToJSON resDto => WebsocketMessage resDto -> AppContextM ()
disconnectUser msg = deleteUser (u' msg.entityId) msg.connectionUuid
