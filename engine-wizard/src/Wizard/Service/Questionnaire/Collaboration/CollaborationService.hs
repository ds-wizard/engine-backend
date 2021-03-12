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
import Shared.Util.Number
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Api.Resource.Websocket.QuestionnaireActionJM ()
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Public
import Wizard.Messaging.Out.Queue.Questionnaire
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
import Wizard.Service.User.UserMapper
import Wizard.Util.Websocket

putUserOnline :: String -> U.UUID -> Connection -> AppContextM ()
putUserOnline qtnUuid connectionUuid connection = do
  myself <- createRecord connectionUuid connection qtnUuid
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
    SetLevelEventChangeDTO' event -> setLevel qtnUuid connectionUuid event
    SetLabelsEventChangeDTO' event -> setLabel qtnUuid connectionUuid event

setReply :: String -> U.UUID -> SetReplyEventChangeDTO -> AppContextM ()
setReply qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let resDto = toSetReplyEventDTO' reqDto mCreatedBy now
  publishToQuestionnaireEventsQueue qtnUuid (SetReplyEventDTO' resDto)
  records <- getAllFromCache
  broadcast qtnUuid records (toSetReplyMessage resDto) disconnectUser

clearReply :: String -> U.UUID -> ClearReplyEventChangeDTO -> AppContextM ()
clearReply qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let resDto = toClearReplyEventDTO' reqDto mCreatedBy now
  publishToQuestionnaireEventsQueue qtnUuid (ClearReplyEventDTO' resDto)
  records <- getAllFromCache
  broadcast qtnUuid records (toClearReplyMessage resDto) disconnectUser

setLevel :: String -> U.UUID -> SetLevelEventChangeDTO -> AppContextM ()
setLevel qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let resDto = toSetLevelEventDTO' reqDto mCreatedBy now
  publishToQuestionnaireEventsQueue qtnUuid (SetLevelEventDTO' resDto)
  records <- getAllFromCache
  broadcast qtnUuid records (toSetLevelMessage resDto) disconnectUser

setLabel :: String -> U.UUID -> SetLabelsEventChangeDTO -> AppContextM ()
setLabel qtnUuid connectionUuid reqDto = do
  myself <- getFromCache' connectionUuid
  checkEditPermission myself
  now <- liftIO getCurrentTime
  let mCreatedBy = getMaybeCreatedBy myself
  let resDto = toSetLabelsEventDTO' reqDto mCreatedBy now
  publishToQuestionnaireEventsQueue qtnUuid (SetLabelsEventDTO' resDto)
  records <- getAllFromCache
  broadcast qtnUuid records (toSetLabelMessage resDto) disconnectUser

-- --------------------------------
-- PRIVATE
-- --------------------------------
disconnectUser :: ToJSON resDto => WebsocketMessage resDto -> AppContextM ()
disconnectUser msg = deleteUser (msg ^. entityId) (msg ^. connectionUuid)

disconnectUserIfLostPermission :: WebsocketRecord -> AppContextM ()
disconnectUserIfLostPermission record = catchError (checkViewPermission record) handleError
  where
    handleError = sendError (record ^. connectionUuid) (record ^. connection) (record ^. entityId) disconnectUser

createRecord :: U.UUID -> Connection -> String -> AppContextM WebsocketRecord
createRecord connectionUuid connection qtnUuid = do
  mCurrentUser <- asks _appContextCurrentUser
  avatarNumber <- liftIO $ generateInt 20
  colorNumber <- liftIO $ generateInt 12
  let user = toOnlineUserInfo mCurrentUser avatarNumber colorNumber
  qtn <- findQuestionnaireById qtnUuid
  mCurrentUser <- asks _appContextCurrentUser
  let permission =
        getPermission
          (qtn ^. visibility)
          (qtn ^. sharing)
          (qtn ^. permissions)
          (mCurrentUser ^? _Just . uuid)
          (mCurrentUser ^? _Just . role)
          (mCurrentUser ^? _Just . groups)
  return $ WebsocketRecord connectionUuid connection qtnUuid permission user

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
