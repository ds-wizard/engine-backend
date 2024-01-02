module Wizard.Service.Questionnaire.Collaboration.CollaborationService where

import Control.Monad (when)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import Network.WebSockets (Connection)

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Api.Resource.Websocket.QuestionnaireActionJM ()
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Cache.QuestionnaireWebsocketCache
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Questionnaire.Collaboration.CollaborationAcl
import Wizard.Service.Questionnaire.Collaboration.CollaborationMapper
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Websocket.WebsocketService
import Wizard.Util.Websocket
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Model.User.UserGroupMembership

putUserOnline :: U.UUID -> U.UUID -> Connection -> AppContextM ()
putUserOnline qtnUuid connectionUuid connection = do
  myself <- createQuestionnaireRecord connectionUuid connection qtnUuid
  checkViewPermission myself
  addToCache myself
  logWS connectionUuid "New user added to the list"
  setUserList qtnUuid connectionUuid

deleteUser :: U.UUID -> U.UUID -> AppContextM ()
deleteUser qtnUuid connectionUuid = do
  deleteFromCache connectionUuid
  setUserList qtnUuid connectionUuid

setUserList :: U.UUID -> U.UUID -> AppContextM ()
setUserList qtnUuid connectionUuid = do
  logWS connectionUuid "Informing other users about user list changes"
  records <- getAllFromCache
  broadcast (U.toString qtnUuid) records (toSetUserListMessage records) disconnectUser
  logWS connectionUuid "Informed completed"

updatePermsForOnlineUsers :: U.UUID -> QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePerm] -> AppContextM ()
updatePermsForOnlineUsers qtnUuid visibility sharing permissions = do
  records <- getAllFromCache
  traverse_ updatePerm records
  where
    updatePerm :: WebsocketRecord -> AppContextM ()
    updatePerm record =
      when
        (record.entityId == U.toString qtnUuid)
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

setQuestionnaire :: U.UUID -> QuestionnaireDetailWsDTO -> AppContextM ()
setQuestionnaire qtnUuid reqDto = do
  logWS U.nil "Informing other users about changed questionnaire"
  records <- getAllFromCache
  broadcast (U.toString qtnUuid) records (toSetQuestionnaireMessage reqDto) disconnectUser
  logWS U.nil "Informed completed"

logOutOnlineUsersWhenQtnDramaticallyChanged :: U.UUID -> AppContextM ()
logOutOnlineUsersWhenQtnDramaticallyChanged qtnUuid = do
  records <- getAllFromCache
  let error = NotExistsError $ _ERROR_SERVICE_QTN_COLLABORATION__FORCE_DISCONNECT (U.toString qtnUuid)
  traverse_ (logOut error) records
  where
    logOut :: AppError -> WebsocketRecord -> AppContextM ()
    logOut error record =
      when
        (record.entityId == U.toString qtnUuid)
        (sendError record.connectionUuid record.connection record.entityId disconnectUser error)

-- --------------------------------
setContent :: U.UUID -> U.UUID -> QuestionnaireEventChangeDTO -> AppContextM ()
setContent qtnUuid connectionUuid reqDto =
  case reqDto of
    SetReplyEventChangeDTO' event -> setReply qtnUuid connectionUuid event
    ClearReplyEventChangeDTO' event -> clearReply qtnUuid connectionUuid event
    SetPhaseEventChangeDTO' event -> setPhase qtnUuid connectionUuid event
    SetLabelsEventChangeDTO' event -> setLabel qtnUuid connectionUuid event
    ResolveCommentThreadEventChangeDTO' event -> resolveCommentThread qtnUuid connectionUuid event
    ReopenCommentThreadEventChangeDTO' event -> reopenCommentThread qtnUuid connectionUuid event
    DeleteCommentThreadEventChangeDTO' event -> deleteCommentThread qtnUuid connectionUuid event
    AddCommentEventChangeDTO' event -> addComment qtnUuid connectionUuid event
    EditCommentEventChangeDTO' event -> editComment qtnUuid connectionUuid event
    DeleteCommentEventChangeDTO' event -> deleteComment qtnUuid connectionUuid event

setReply :: U.UUID -> U.UUID -> SetReplyEventChangeDTO -> AppContextM ()
setReply qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid
    qtnUuid
    [fromEventChangeDTO (SetReplyEventChangeDTO' reqDto) mCreatedByUuid now]
    reqDto.phasesAnsweredIndication
  let resDto = toSetReplyEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString qtnUuid) records (toSetReplyMessage resDto) disconnectUser

clearReply :: U.UUID -> U.UUID -> ClearReplyEventChangeDTO -> AppContextM ()
clearReply qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid
    qtnUuid
    [fromEventChangeDTO (ClearReplyEventChangeDTO' reqDto) mCreatedByUuid now]
    reqDto.phasesAnsweredIndication
  let resDto = toClearReplyEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString qtnUuid) records (toClearReplyMessage resDto) disconnectUser

setPhase :: U.UUID -> U.UUID -> SetPhaseEventChangeDTO -> AppContextM ()
setPhase qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid
    qtnUuid
    [fromEventChangeDTO (SetPhaseEventChangeDTO' reqDto) mCreatedByUuid now]
    reqDto.phasesAnsweredIndication
  let resDto = toSetPhaseEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString qtnUuid) records (toSetPhaseMessage resDto) disconnectUser

setLabel :: U.UUID -> U.UUID -> SetLabelsEventChangeDTO -> AppContextM ()
setLabel qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid' qtnUuid [fromEventChangeDTO (SetLabelsEventChangeDTO' reqDto) mCreatedByUuid now]
  let resDto = toSetLabelsEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast (U.toString qtnUuid) records (toSetLabelMessage resDto) disconnectUser

resolveCommentThread :: U.UUID -> U.UUID -> ResolveCommentThreadEventChangeDTO -> AppContextM ()
resolveCommentThread qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  updateQuestionnaireCommentThreadResolvedById reqDto.threadUuid True
  let resDto = toResolveCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else records
  broadcast (U.toString qtnUuid) filteredRecords (toResolveCommentThreadMessage resDto) disconnectUser

reopenCommentThread :: U.UUID -> U.UUID -> ReopenCommentThreadEventChangeDTO -> AppContextM ()
reopenCommentThread qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  updateQuestionnaireCommentThreadResolvedById reqDto.threadUuid False
  let resDto = toReopenCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else records
  broadcast (U.toString qtnUuid) filteredRecords (toReopenCommentThreadMessage resDto) disconnectUser

deleteCommentThread :: U.UUID -> U.UUID -> DeleteCommentThreadEventChangeDTO -> AppContextM ()
deleteCommentThread qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  deleteQuestionnaireCommentsByThreadUuid reqDto.threadUuid
  deleteQuestionnaireCommentThreadById reqDto.threadUuid
  let resDto = toDeleteCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else records
  broadcast (U.toString qtnUuid) filteredRecords (toDeleteCommentThreadMessage resDto) disconnectUser

addComment :: U.UUID -> U.UUID -> AddCommentEventChangeDTO -> AppContextM ()
addComment qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  tenantUuid <- asks currentTenantUuid
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  let comment = toComment reqDto tenantUuid mCreatedByUuid now
  if reqDto.newThread
    then do
      let thread = toCommentThread reqDto qtnUuid tenantUuid mCreatedByUuid now
      insertQuestionnaireThreadAndComment thread comment
    else insertQuestionnaireComment comment
  let resDto = toAddCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else records
  broadcast (U.toString qtnUuid) filteredRecords (toAddCommentMessage resDto) disconnectUser

editComment :: U.UUID -> U.UUID -> EditCommentEventChangeDTO -> AppContextM ()
editComment qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  updateQuestionnaireCommentTextById reqDto.commentUuid reqDto.text
  let resDto = toEditCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else records
  broadcast (U.toString qtnUuid) filteredRecords (toEditCommentMessage resDto) disconnectUser

deleteComment :: U.UUID -> U.UUID -> DeleteCommentEventChangeDTO -> AppContextM ()
deleteComment qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  deleteQuestionnaireCommentById reqDto.commentUuid
  let resDto = toDeleteCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto.private
          then filterEditors records
          else records
  broadcast (U.toString qtnUuid) filteredRecords (toDeleteCommentMessage resDto) disconnectUser

-- --------------------------------
-- PRIVATE
-- --------------------------------
disconnectUser :: ToJSON resDto => WebsocketMessage resDto -> AppContextM ()
disconnectUser msg = deleteUser (u' msg.entityId) msg.connectionUuid

disconnectUserIfLostPermission :: WebsocketRecord -> AppContextM ()
disconnectUserIfLostPermission record = catchError (checkViewPermission record) handleError
  where
    handleError = sendError record.connectionUuid record.connection record.entityId disconnectUser

createQuestionnaireRecord :: U.UUID -> Connection -> U.UUID -> AppContextM WebsocketRecord
createQuestionnaireRecord connectionUuid connection qtnUuid = do
  mCurrentUser <- asks currentUser
  qtn <- findQuestionnaireByUuid qtnUuid
  userGroupUuids <-
    case mCurrentUser of
      Just currentUser -> do
        userGroupMemberships <- findUserGroupMembershipsByUserUuid currentUser.uuid
        return . fmap (.userGroupUuid) $ userGroupMemberships
      Nothing -> return []
  let permission =
        getPermission
          qtn.visibility
          qtn.sharing
          qtn.permissions
          (fmap (.uuid) mCurrentUser)
          (fmap (.uRole) mCurrentUser)
          userGroupUuids
  createRecord connectionUuid connection (U.toString qtnUuid) permission userGroupUuids

getMaybeCreatedBy :: WebsocketRecord -> Maybe UserSuggestionDTO
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
          UserSuggestionDTO
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
