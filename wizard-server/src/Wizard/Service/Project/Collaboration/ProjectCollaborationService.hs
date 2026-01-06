module Wizard.Service.Project.Collaboration.ProjectCollaborationService where

import Control.Monad (when)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.Time
import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Shared.Common.Integration.Aws.Lambda
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Wizard.Api.Resource.Project.Event.ProjectEventChangeDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.Websocket.ProjectMessageJM ()
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Cache.ProjectWebsocketCache
import Wizard.Database.DAO.Project.ProjectCommentDAO
import Wizard.Database.DAO.Project.ProjectCommentThreadDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.File.ProjectFileSimple
import Wizard.Model.Project.Project
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Project.Collaboration.ProjectCollaborationAcl
import Wizard.Service.Project.Collaboration.ProjectCollaborationMapper
import Wizard.Service.Project.Comment.ProjectCommentMapper
import Wizard.Service.Project.Event.ProjectEventMapper
import Wizard.Service.Websocket.WebsocketService
import Wizard.Util.Websocket
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Model.User.UserGroupMembership
import WizardLib.Public.Model.User.UserSuggestion

putUserOnline :: U.UUID -> U.UUID -> Connection -> AppContextM ()
putUserOnline projectUuid connectionUuid connection = do
  myself <- createProjectRecord connectionUuid connection projectUuid
  checkViewPermission myself
  addToCache myself
  logWS connectionUuid "New user added to the list"
  setUserList projectUuid connectionUuid

deleteUser :: U.UUID -> U.UUID -> AppContextM ()
deleteUser projectUuid connectionUuid = do
  deleteFromCache connectionUuid
  setUserList projectUuid connectionUuid

setUserList :: U.UUID -> U.UUID -> AppContextM ()
setUserList projectUuid connectionUuid = do
  logWS connectionUuid "Informing other users about user list changes"
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toSetUserListMessage records) disconnectUser
  logWS connectionUuid "Informed completed"

updatePermsForOnlineUsers :: U.UUID -> ProjectVisibility -> ProjectSharing -> [ProjectPerm] -> AppContextM ()
updatePermsForOnlineUsers projectUuid visibility sharing permissions = do
  currentTenantUuid <- asks currentTenantUuid
  tenant <- findTenantByUuid currentTenantUuid
  if isJust tenant.signalBridgeUrl
    then do
      serverConfig <- asks serverConfig
      let dto = AKM.fromList [("projectUuid", U.toString projectUuid), ("tenantUuid", U.toString currentTenantUuid)]
      invokeLambda serverConfig.signalBridge.updatePermsArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      records <- getAllFromCache
      traverse_ updatePerm records
  where
    updatePerm :: WebsocketRecord -> AppContextM ()
    updatePerm record =
      when
        (record.entityId == U.toString projectUuid)
        ( do
            let permission =
                  case record.user of
                    user@LoggedOnlineUserInfo {uuid = uuid, role = role, groupUuids = groupUuids} ->
                      getPermission visibility sharing permissions (Just uuid) (Just role) groupUuids
                    user@AnonymousOnlineUserInfo {..} ->
                      getPermission visibility sharing permissions Nothing Nothing []
            let updatedRecord = record {entityPerm = permission}
            updateCache updatedRecord
            disconnectUserIfLostPermission updatedRecord
        )

removeUserGroupFromUsers :: U.UUID -> [U.UUID] -> AppContextM ()
removeUserGroupFromUsers userGroupUuid userUuids = do
  currentTenantUuid <- asks currentTenantUuid
  tenant <- findTenantByUuid currentTenantUuid
  if isJust tenant.signalBridgeUrl
    then do
      serverConfig <- asks serverConfig
      let dto = AKM.fromList [("userGroupUuid", U.toString userGroupUuid), ("tenantUuid", U.toString currentTenantUuid)]
      invokeLambda serverConfig.signalBridge.updateUserGroupArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      records <- getAllFromCache
      traverse_ updatePerm records
  where
    updatePerm :: WebsocketRecord -> AppContextM ()
    updatePerm record =
      case record.user of
        user@LoggedOnlineUserInfo {uuid = uuid, role = role, groupUuids = groupUuids} -> do
          when
            (user.uuid `elem` userUuids)
            ( do
                let updatedRecord = record {user = user {groupUuids = filter (/= userGroupUuid) user.groupUuids}}
                updateCache updatedRecord
            )
        user@AnonymousOnlineUserInfo {..} -> return ()

setProject :: U.UUID -> ProjectDetailWsDTO -> AppContextM ()
setProject projectUuid reqDto = do
  currentTenantUuid <- asks currentTenantUuid
  tenant <- findTenantByUuid currentTenantUuid
  if isJust tenant.signalBridgeUrl
    then do
      serverConfig <- asks serverConfig
      let dto =
            AKM.fromList
              [ ("projectUuid", A.String . U.toText $ projectUuid)
              , ("tenantUuid", A.String . U.toText $ currentTenantUuid)
              , ("message", A.toJSON reqDto)
              ]
      invokeLambda serverConfig.signalBridge.setProjectArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      logWS U.nil "Informing other users about changed project"
      records <- getAllFromCache
      broadcast (U.toString projectUuid) records (toSetProjectMessage reqDto) disconnectUser
      logWS U.nil "Informed completed"

addFile :: U.UUID -> ProjectFileSimple -> AppContextM ()
addFile projectUuid reqDto = do
  currentTenantUuid <- asks currentTenantUuid
  tenant <- findTenantByUuid currentTenantUuid
  if isJust tenant.signalBridgeUrl
    then do
      serverConfig <- asks serverConfig
      let dto =
            AKM.fromList
              [ ("projectUuid", A.String . U.toText $ projectUuid)
              , ("tenantUuid", A.String . U.toText $ currentTenantUuid)
              , ("message", A.toJSON reqDto)
              ]
      invokeLambda serverConfig.signalBridge.addFileArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      logWS U.nil "Informing other users about added file"
      records <- getAllFromCache
      broadcast (U.toString projectUuid) records (toAddFileMessage reqDto) disconnectUser
      logWS U.nil "Informed completed"

logOutOnlineUsersWhenProjectDramaticallyChanged :: U.UUID -> AppContextM ()
logOutOnlineUsersWhenProjectDramaticallyChanged projectUuid = do
  currentTenantUuid <- asks currentTenantUuid
  tenant <- findTenantByUuid currentTenantUuid
  if isJust tenant.signalBridgeUrl
    then do
      serverConfig <- asks serverConfig
      let dto = AKM.fromList [("projectUuid", U.toString projectUuid), ("tenantUuid", U.toString currentTenantUuid)]
      invokeLambda serverConfig.signalBridge.logOutAllArn (BSL.toStrict . A.encode $ dto)
      return ()
    else do
      records <- getAllFromCache
      let error = NotExistsError $ _ERROR_SERVICE_PROJECT_COLLABORATION__FORCE_DISCONNECT (U.toString projectUuid)
      traverse_ (logOut error) records
  where
    logOut :: AppError -> WebsocketRecord -> AppContextM ()
    logOut error record =
      when
        (record.entityId == U.toString projectUuid)
        (sendError record.connectionUuid record.connection record.entityId disconnectUser error)

-- --------------------------------
setContent :: U.UUID -> U.UUID -> ProjectEventChangeDTO -> AppContextM ()
setContent projectUuid connectionUuid reqDto =
  case reqDto of
    SetReplyEventChangeDTO' event -> setReply projectUuid connectionUuid event
    ClearReplyEventChangeDTO' event -> clearReply projectUuid connectionUuid event
    SetPhaseEventChangeDTO' event -> setPhase projectUuid connectionUuid event
    SetLabelsEventChangeDTO' event -> setLabel projectUuid connectionUuid event
    ResolveCommentThreadEventChangeDTO' event -> resolveCommentThread projectUuid connectionUuid event
    ReopenCommentThreadEventChangeDTO' event -> reopenCommentThread projectUuid connectionUuid event
    AssignCommentThreadEventChangeDTO' event -> assignCommentThread projectUuid connectionUuid event
    DeleteCommentThreadEventChangeDTO' event -> deleteCommentThread projectUuid connectionUuid event
    AddCommentEventChangeDTO' event -> addComment projectUuid connectionUuid event
    EditCommentEventChangeDTO' event -> editComment projectUuid connectionUuid event
    DeleteCommentEventChangeDTO' event -> deleteComment projectUuid connectionUuid event

setReply :: U.UUID -> U.UUID -> SetReplyEventChangeDTO -> AppContextM ()
setReply projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  tenantUuid <- asks currentTenantUuid
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  insertProjectEventWithTimestampUpdate
    projectUuid
    (fromEventChangeDTO (SetReplyEventChangeDTO' reqDto) projectUuid tenantUuid mCreatedByUuid now)
  let resDto = toSetReplyEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toSetReplyMessage resDto) disconnectUser

clearReply :: U.UUID -> U.UUID -> ClearReplyEventChangeDTO -> AppContextM ()
clearReply projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  tenantUuid <- asks currentTenantUuid
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  insertProjectEventWithTimestampUpdate
    projectUuid
    (fromEventChangeDTO (ClearReplyEventChangeDTO' reqDto) projectUuid tenantUuid mCreatedByUuid now)
  let resDto = toClearReplyEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toClearReplyMessage resDto) disconnectUser

setPhase :: U.UUID -> U.UUID -> SetPhaseEventChangeDTO -> AppContextM ()
setPhase projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  tenantUuid <- asks currentTenantUuid
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  insertProjectEventWithTimestampUpdate
    projectUuid
    (fromEventChangeDTO (SetPhaseEventChangeDTO' reqDto) projectUuid tenantUuid mCreatedByUuid now)
  let resDto = toSetPhaseEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toSetPhaseMessage resDto) disconnectUser

setLabel :: U.UUID -> U.UUID -> SetLabelsEventChangeDTO -> AppContextM ()
setLabel projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  tenantUuid <- asks currentTenantUuid
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  insertProjectEventWithTimestampUpdate projectUuid (fromEventChangeDTO (SetLabelsEventChangeDTO' reqDto) projectUuid tenantUuid mCreatedByUuid now)
  let resDto = toSetLabelsEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString projectUuid) records (toSetLabelMessage resDto) disconnectUser

resolveCommentThread :: U.UUID -> U.UUID -> ResolveCommentThreadEventChangeDTO -> AppContextM ()
resolveCommentThread projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  updateProjectCommentThreadResolvedById reqDto.threadUuid True
  let resDto = toResolveCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toResolveCommentThreadMessage resDto) disconnectUser

reopenCommentThread :: U.UUID -> U.UUID -> ReopenCommentThreadEventChangeDTO -> AppContextM ()
reopenCommentThread projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  updateProjectCommentThreadResolvedById reqDto.threadUuid False
  let resDto = toReopenCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toReopenCommentThreadMessage resDto) disconnectUser

assignCommentThread :: U.UUID -> U.UUID -> AssignCommentThreadEventChangeDTO -> AppContextM ()
assignCommentThread projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  updateProjectCommentThreadAssignee reqDto.threadUuid (fmap (.uuid) reqDto.assignedTo) mCreatedByUuid
  let resDto = toAssignCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toAssignCommentThreadMessage resDto) disconnectUser

deleteCommentThread :: U.UUID -> U.UUID -> DeleteCommentThreadEventChangeDTO -> AppContextM ()
deleteCommentThread projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  deleteProjectCommentsByThreadUuid reqDto.threadUuid
  deleteProjectCommentThreadById reqDto.threadUuid
  let resDto = toDeleteCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toDeleteCommentThreadMessage resDto) disconnectUser

addComment :: U.UUID -> U.UUID -> AddCommentEventChangeDTO -> AppContextM ()
addComment projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  tenantUuid <- asks currentTenantUuid
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  let comment = toComment reqDto tenantUuid mCreatedByUuid now
  if reqDto.newThread
    then do
      let thread = toCommentThread reqDto projectUuid tenantUuid mCreatedByUuid now
      insertProjectThreadAndComment thread comment
    else insertProjectComment comment
  let resDto = toAddCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toAddCommentMessage resDto) disconnectUser

editComment :: U.UUID -> U.UUID -> EditCommentEventChangeDTO -> AppContextM ()
editComment projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  updateProjectCommentTextById reqDto.commentUuid reqDto.text
  let resDto = toEditCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toEditCommentMessage resDto) disconnectUser

deleteComment :: U.UUID -> U.UUID -> DeleteCommentEventChangeDTO -> AppContextM ()
deleteComment projectUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  deleteProjectCommentById reqDto.commentUuid
  let resDto = toDeleteCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else filterCommenters records
  broadcast (U.toString projectUuid) filteredRecords (toDeleteCommentMessage resDto) disconnectUser

-- --------------------------------
-- PRIVATE
-- --------------------------------
disconnectUser :: A.ToJSON resDto => WebsocketMessage resDto -> AppContextM ()
disconnectUser msg = deleteUser (u' msg.entityId) msg.connectionUuid

disconnectUserIfLostPermission :: WebsocketRecord -> AppContextM ()
disconnectUserIfLostPermission record = catchError (checkViewPermission record) handleError
  where
    handleError = sendError record.connectionUuid record.connection record.entityId disconnectUser

createProjectRecord :: U.UUID -> Connection -> U.UUID -> AppContextM WebsocketRecord
createProjectRecord connectionUuid connection projectUuid = do
  mCurrentUser <- asks currentUser
  project <- findProjectByUuid projectUuid
  userGroupUuids <-
    case mCurrentUser of
      Just currentUser -> do
        userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
        return . fmap (.userGroupUuid) $ userGroupMemberships
      Nothing -> return []
  let permission =
        getPermission
          project.visibility
          project.sharing
          project.permissions
          (fmap (.uuid) mCurrentUser)
          (fmap (.uRole) mCurrentUser)
          userGroupUuids
  createRecord connectionUuid connection (U.toString projectUuid) permission userGroupUuids

getMaybeCreatedBy :: WebsocketRecord -> Maybe UserSuggestion
getMaybeCreatedBy myself =
  case myself.user of
    u@LoggedOnlineUserInfo
      { uuid = uuid
      , firstName = firstName
      , lastName = lastName
      , gravatarHash = gravatarHash
      , imageUrl = imageUrl
      } ->
        Just $
          UserSuggestion
            { uuid = uuid
            , firstName = firstName
            , lastName = lastName
            , gravatarHash = gravatarHash
            , imageUrl = imageUrl
            }
    u@AnonymousOnlineUserInfo {..} -> Nothing

getMaybeCreatedByUuid :: WebsocketRecord -> Maybe U.UUID
getMaybeCreatedByUuid myself =
  case myself.user of
    u@LoggedOnlineUserInfo {uuid = uuid} -> Just uuid
    u@AnonymousOnlineUserInfo {..} -> Nothing
