module Wizard.Service.Questionnaire.Collaboration.CollaborationService where

import Control.Lens ((&), (.~), (^.), (^?), _Just)
import Control.Monad (when)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import Network.WebSockets (Connection)

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Api.Resource.Websocket.QuestionnaireActionJM ()
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.User.OnlineUserInfo
import Wizard.Model.Websocket.WebsocketMessage
import Wizard.Model.Websocket.WebsocketRecord
import Wizard.Service.Cache.QuestionnaireWebsocketCache
import Wizard.Service.Questionnaire.Collaboration.CollaborationAcl
import Wizard.Service.Questionnaire.Collaboration.CollaborationMapper
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Websocket.WebsocketService
import Wizard.Util.Websocket

putUserOnline :: String -> U.UUID -> Connection -> AppContextM ()
putUserOnline qtnUuid connectionUuid connection = do
  myself <- createQuestionnaireRecord connectionUuid connection qtnUuid
  checkViewPermission myself
  addToCache myself
  logWS connectionUuid "New user added to the list"
  setUserList qtnUuid connectionUuid

deleteUser :: String -> U.UUID -> AppContextM ()
deleteUser qtnUuid connectionUuid = do
  deleteFromCache connectionUuid
  setUserList qtnUuid connectionUuid

setUserList :: String -> U.UUID -> AppContextM ()
setUserList qtnUuid connectionUuid = do
  logWS connectionUuid "Informing other users about user list changes"
  records <- getAllFromCache
  broadcast qtnUuid records (toSetUserListMessage records) disconnectUser
  logWS connectionUuid "Informed completed"

updatePermsForOnlineUsers ::
     String -> QuestionnaireVisibility -> QuestionnaireSharing -> [QuestionnairePermRecord] -> AppContextM ()
updatePermsForOnlineUsers qtnUuid visibility sharing permissions = do
  records <- getAllFromCache
  traverse_ updatePerm records
  where
    updatePerm :: WebsocketRecord -> AppContextM ()
    updatePerm record =
      when
        (record ^. entityId == qtnUuid)
        (do let permission =
                  case record ^. user of
                    user@LoggedOnlineUserInfo { _loggedOnlineUserInfoUuid = uuid
                                              , _loggedOnlineUserInfoRole = role
                                              , _loggedOnlineUserInfoGroups = groups
                                              } ->
                      getPermission visibility sharing permissions (Just uuid) (Just role) (Just groups)
                    user@AnonymousOnlineUserInfo {..} ->
                      getPermission visibility sharing permissions Nothing Nothing Nothing
            let updatedRecord = record & entityPerm .~ permission
            updateCache updatedRecord
            disconnectUserIfLostPermission updatedRecord)

logOutOnlineUsersWhenQtnDramaticallyChanged :: String -> AppContextM ()
logOutOnlineUsersWhenQtnDramaticallyChanged qtnUuid = do
  records <- getAllFromCache
  let error = NotExistsError $ _ERROR_SERVICE_QTN_COLLABORATION__FORCE_DISCONNECT qtnUuid
  traverse_ (logOut error) records
  where
    logOut :: AppError -> WebsocketRecord -> AppContextM ()
    logOut error record =
      when
        (record ^. entityId == qtnUuid)
        (sendError (record ^. connectionUuid) (record ^. connection) (record ^. entityId) disconnectUser error)

-- --------------------------------
setContent :: String -> U.UUID -> QuestionnaireEventChangeDTO -> AppContextM ()
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

setReply :: String -> U.UUID -> SetReplyEventChangeDTO -> AppContextM ()
setReply qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid
    qtnUuid
    [fromEventChangeDTO (SetReplyEventChangeDTO' reqDto) mCreatedByUuid now]
    (reqDto ^. phasesAnsweredIndication)
  let resDto = toSetReplyEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast qtnUuid records (toSetReplyMessage resDto) disconnectUser

clearReply :: String -> U.UUID -> ClearReplyEventChangeDTO -> AppContextM ()
clearReply qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid
    qtnUuid
    [fromEventChangeDTO (ClearReplyEventChangeDTO' reqDto) mCreatedByUuid now]
    (reqDto ^. phasesAnsweredIndication)
  let resDto = toClearReplyEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast qtnUuid records (toClearReplyMessage resDto) disconnectUser

setPhase :: String -> U.UUID -> SetPhaseEventChangeDTO -> AppContextM ()
setPhase qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid
    qtnUuid
    [fromEventChangeDTO (SetPhaseEventChangeDTO' reqDto) mCreatedByUuid now]
    (reqDto ^. phasesAnsweredIndication)
  let resDto = toSetPhaseEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast qtnUuid records (toSetPhaseMessage resDto) disconnectUser

setLabel :: String -> U.UUID -> SetLabelsEventChangeDTO -> AppContextM ()
setLabel qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid
    qtnUuid
    [fromEventChangeDTO (SetLabelsEventChangeDTO' reqDto) mCreatedByUuid now]
    (reqDto ^. phasesAnsweredIndication)
  let resDto = toSetLabelsEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  broadcast qtnUuid records (toSetLabelMessage resDto) disconnectUser

resolveCommentThread :: String -> U.UUID -> ResolveCommentThreadEventChangeDTO -> AppContextM ()
resolveCommentThread qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid'
    qtnUuid
    [fromEventChangeDTO (ResolveCommentThreadEventChangeDTO' reqDto) mCreatedByUuid now]
  let resDto = toResolveCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto ^. private
          then filterEditors records
          else records
  broadcast qtnUuid filteredRecords (toResolveCommentThreadMessage resDto) disconnectUser

reopenCommentThread :: String -> U.UUID -> ReopenCommentThreadEventChangeDTO -> AppContextM ()
reopenCommentThread qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid'
    qtnUuid
    [fromEventChangeDTO (ReopenCommentThreadEventChangeDTO' reqDto) mCreatedByUuid now]
  let resDto = toReopenCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto ^. private
          then filterEditors records
          else records
  broadcast qtnUuid filteredRecords (toReopenCommentThreadMessage resDto) disconnectUser

deleteCommentThread :: String -> U.UUID -> DeleteCommentThreadEventChangeDTO -> AppContextM ()
deleteCommentThread qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid'
    qtnUuid
    [fromEventChangeDTO (DeleteCommentThreadEventChangeDTO' reqDto) mCreatedByUuid now]
  let resDto = toDeleteCommentThreadEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto ^. private
          then filterEditors records
          else records
  broadcast qtnUuid filteredRecords (toDeleteCommentThreadMessage resDto) disconnectUser

addComment :: String -> U.UUID -> AddCommentEventChangeDTO -> AppContextM ()
addComment qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid' qtnUuid [fromEventChangeDTO (AddCommentEventChangeDTO' reqDto) mCreatedByUuid now]
  let resDto = toAddCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto ^. private
          then filterEditors records
          else records
  broadcast qtnUuid filteredRecords (toAddCommentMessage resDto) disconnectUser

editComment :: String -> U.UUID -> EditCommentEventChangeDTO -> AppContextM ()
editComment qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid' qtnUuid [fromEventChangeDTO (EditCommentEventChangeDTO' reqDto) mCreatedByUuid now]
  let resDto = toEditCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto ^. private
          then filterEditors records
          else records
  broadcast qtnUuid filteredRecords (toEditCommentMessage resDto) disconnectUser

deleteComment :: String -> U.UUID -> DeleteCommentEventChangeDTO -> AppContextM ()
deleteComment qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkCommentPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let mCreatedByUuid = getMaybeCreatedByUuid myself
  appendQuestionnaireEventByUuid' qtnUuid [fromEventChangeDTO (DeleteCommentEventChangeDTO' reqDto) mCreatedByUuid now]
  let resDto = toDeleteCommentEventDTO' reqDto mCreatedBy now
  records <- getAllFromCache
  let filteredRecords =
        if reqDto ^. private
          then filterEditors records
          else records
  broadcast qtnUuid filteredRecords (toDeleteCommentMessage resDto) disconnectUser

-- --------------------------------
-- PRIVATE
-- --------------------------------
disconnectUser :: ToJSON resDto => WebsocketMessage resDto -> AppContextM ()
disconnectUser msg = deleteUser (msg ^. entityId) (msg ^. connectionUuid)

disconnectUserIfLostPermission :: WebsocketRecord -> AppContextM ()
disconnectUserIfLostPermission record = catchError (checkViewPermission record) handleError
  where
    handleError = sendError (record ^. connectionUuid) (record ^. connection) (record ^. entityId) disconnectUser

createQuestionnaireRecord :: U.UUID -> Connection -> String -> AppContextM WebsocketRecord
createQuestionnaireRecord connectionUuid connection qtnUuid = do
  mCurrentUser <- asks _appContextCurrentUser
  qtn <- findQuestionnaireById qtnUuid
  let permission =
        getPermission
          (qtn ^. visibility)
          (qtn ^. sharing)
          (qtn ^. permissions)
          (mCurrentUser ^? _Just . uuid)
          (mCurrentUser ^? _Just . role)
          (mCurrentUser ^? _Just . groups)
  createRecord connectionUuid connection qtnUuid permission

getMaybeCreatedBy :: WebsocketRecord -> Maybe UserSuggestionDTO
getMaybeCreatedBy myself =
  case myself ^. user of
    u@LoggedOnlineUserInfo { _loggedOnlineUserInfoUuid = uuid
                           , _loggedOnlineUserInfoFirstName = firstName
                           , _loggedOnlineUserInfoLastName = lastName
                           , _loggedOnlineUserInfoGravatarHash = gravatarHash
                           , _loggedOnlineUserInfoImageUrl = imageUrl
                           } ->
      Just $
      UserSuggestionDTO
        { _userSuggestionDTOUuid = uuid
        , _userSuggestionDTOFirstName = firstName
        , _userSuggestionDTOLastName = lastName
        , _userSuggestionDTOGravatarHash = gravatarHash
        , _userSuggestionDTOImageUrl = imageUrl
        }
    u@AnonymousOnlineUserInfo {..} -> Nothing

getMaybeCreatedByUuid :: WebsocketRecord -> Maybe U.UUID
getMaybeCreatedByUuid myself =
  case myself ^. user of
    u@LoggedOnlineUserInfo {_loggedOnlineUserInfoUuid = uuid} -> Just uuid
    u@AnonymousOnlineUserInfo {..} -> Nothing
