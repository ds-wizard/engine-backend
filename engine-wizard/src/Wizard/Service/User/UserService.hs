module Wizard.Service.User.UserService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (forM_, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Error.Error
import Shared.Util.Crypto (generateRandomString)
import Shared.Util.Uuid
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.ActionKey.ActionKey
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Model.User.UserEM ()
import Wizard.S3.Document.DocumentS3
import Wizard.Service.Acl.AclService
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Common
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Mail.Mailer
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserValidation

getUsers :: AppContextM [UserDTO]
getUsers =
  runInTransaction $ do
    checkPermission _UM_PERM
    users <- findUsers
    return . fmap toDTO $ users

getUsersPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page UserDTO)
getUsersPage mQuery mRole pageable sort =
  runInTransaction $ do
    checkPermission _UM_PERM
    userPage <- findUsersPage mQuery mRole pageable sort
    return . fmap toDTO $ userPage

getUserSuggestionsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page UserSuggestionDTO)
getUserSuggestionsPage mQuery pageable sort =
  runInTransaction $ do
    suggestionPage <- findUserSuggestionsPage mQuery pageable sort
    return . fmap toSuggestionDTO $ suggestionPage

createUserByAdmin :: UserCreateDTO -> AppContextM UserDTO
createUserByAdmin reqDto =
  runInTransaction $ do
    uUuid <- liftIO generateUuid
    createUserByAdminWithUuid reqDto uUuid

createUserByAdminWithUuid :: UserCreateDTO -> U.UUID -> AppContextM UserDTO
createUserByAdminWithUuid reqDto uUuid =
  runInTransaction $ do
    checkPermission _UM_PERM
    uPasswordHash <- generatePasswordHash (reqDto ^. password)
    serverConfig <- asks _appContextServerConfig
    appConfig <- getAppConfig
    let uRole = fromMaybe (appConfig ^. authentication . defaultRole) (reqDto ^. role)
    let uPermissions = getPermissionForRole serverConfig uRole
    createUser reqDto uUuid uPasswordHash uRole uPermissions

registrateUser :: UserCreateDTO -> AppContextM UserDTO
registrateUser reqDto =
  runInTransaction $ do
    checkIfRegistrationIsEnabled
    uUuid <- liftIO generateUuid
    uPasswordHash <- generatePasswordHash (reqDto ^. password)
    serverConfig <- asks _appContextServerConfig
    appConfig <- getAppConfig
    let uRole = appConfig ^. authentication . defaultRole
    let uPermissions = getPermissionForRole serverConfig uRole
    createUser reqDto uUuid uPasswordHash uRole uPermissions

createUser :: UserCreateDTO -> U.UUID -> String -> String -> [String] -> AppContextM UserDTO
createUser reqDto uUuid uPasswordHash uRole uPermissions =
  runInTransaction $ do
    validateUserEmailUniqueness (reqDto ^. email)
    now <- liftIO getCurrentTime
    let user = fromUserCreateDTO reqDto uUuid uPasswordHash uRole uPermissions now
    insertUser user
    actionKey <- createActionKey uUuid RegistrationActionKey
    catchError
      (sendRegistrationConfirmationMail (toDTO user) (actionKey ^. hash))
      (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT)
    sendAnalyticsEmailIfEnabled user
    return $ toDTO user

createUserFromExternalService :: String -> String -> String -> String -> Maybe String -> AppContextM UserDTO
createUserFromExternalService serviceId firstName lastName email mImageUrl =
  runInTransaction $ do
    mUserFromDb <- findUserByEmail' email
    now <- liftIO getCurrentTime
    case mUserFromDb of
      Just user ->
        if user ^. active
          then do
            let updatedUser = fromUpdateUserExternalDTO user firstName lastName mImageUrl serviceId now
            updateUserById updatedUser
            return $ toDTO updatedUser
          else throwError $ UserError _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED
      Nothing -> do
        serverConfig <- asks _appContextServerConfig
        uUuid <- liftIO generateUuid
        password <- liftIO $ generateRandomString 40
        uPasswordHash <- generatePasswordHash password
        appConfig <- getAppConfig
        let uRole = appConfig ^. authentication . defaultRole
        let uPerms = getPermissionForRole serverConfig uRole
        let user =
              fromUserExternalDTO uUuid firstName lastName email uPasswordHash [serviceId] uRole uPerms mImageUrl now
        insertUser user
        sendAnalyticsEmailIfEnabled user
        return $ toDTO user

getUserById :: String -> AppContextM UserDTO
getUserById userUuid =
  runInTransaction $ do
    user <- findUserById userUuid
    return $ toDTO user

getUserDetailById :: String -> AppContextM UserDTO
getUserDetailById userUuid =
  runInTransaction $ do
    checkPermission _UM_PERM
    getUserById userUuid

modifyUser :: String -> UserChangeDTO -> AppContextM UserDTO
modifyUser userUuid reqDto =
  runInTransaction $ do
    checkPermission _UM_PERM
    user <- findUserById userUuid
    validateUserChangedEmailUniqueness (reqDto ^. email) (user ^. email)
    serverConfig <- asks _appContextServerConfig
    updatedUser <- updateUserTimestamp $ fromUserChangeDTO reqDto user (getPermissions serverConfig reqDto user)
    updateUserById updatedUser
    return . toDTO $ updatedUser
  where
    getPermissions serverConfig reqDto oldUser =
      if (reqDto ^. role) /= (oldUser ^. role)
        then getPermissionForRole serverConfig (reqDto ^. role)
        else oldUser ^. permissions

changeUserPasswordByAdmin :: String -> UserPasswordDTO -> AppContextM ()
changeUserPasswordByAdmin userUuid reqDto =
  runInTransaction $ do
    user <- findUserById userUuid
    passwordHash <- generatePasswordHash (reqDto ^. password)
    now <- liftIO getCurrentTime
    updateUserPasswordById userUuid passwordHash now
    return ()

changeUserPasswordByHash :: String -> Maybe String -> UserPasswordDTO -> AppContextM ()
changeUserPasswordByHash userUuid maybeHash userPasswordDto =
  runInTransaction $ do
    actionKey <- getActionKeyByHash maybeHash
    user <- findUserById (U.toString $ actionKey ^. userId)
    passwordHash <- generatePasswordHash (userPasswordDto ^. password)
    now <- liftIO getCurrentTime
    updateUserPasswordById userUuid passwordHash now
    deleteActionKeyByHash (actionKey ^. hash)
    return ()

resetUserPassword :: ActionKeyDTO -> AppContextM ()
resetUserPassword reqDto =
  runInTransaction $ do
    user <- findUserByEmail (reqDto ^. email)
    actionKey <- createActionKey (user ^. uuid) ForgottenPasswordActionKey
    catchError
      (sendResetPasswordMail (toDTO user) (actionKey ^. hash))
      (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT)

changeUserState :: String -> Maybe String -> UserStateDTO -> AppContextM UserStateDTO
changeUserState userUuid maybeHash userStateDto =
  runInTransaction $ do
    actionKey <- getActionKeyByHash maybeHash
    user <- findUserById (U.toString $ actionKey ^. userId)
    updatedUser <- updateUserTimestamp $ user & active .~ (userStateDto ^. active)
    updateUserById updatedUser
    deleteActionKeyByHash (actionKey ^. hash)
    return userStateDto

deleteUser :: String -> AppContextM ()
deleteUser userUuid =
  runInTransaction $ do
    checkPermission _UM_PERM
    user <- findUserById userUuid
    removeOwnerFromQuestionnaire (user ^. uuid)
    documents <- findDocumentsFiltered [("creator_uuid", userUuid)]
    forM_
      documents
      (\d -> do
         deleteDocumentsFiltered [("uuid", U.toString $ d ^. uuid)]
         removeDocumentContent (U.toString $ d ^. uuid))
    deleteUserById userUuid
    return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPermissionForRole :: ServerConfig -> String -> [String]
getPermissionForRole config role
  | role == _USER_ROLE_ADMIN = config ^. roles . admin
  | role == _USER_ROLE_DATA_STEWARD = config ^. roles . dataSteward
  | role == _USER_ROLE_RESEARCHER = config ^. roles . researcher
  | otherwise = []

generatePasswordHash :: String -> AppContextM String
generatePasswordHash password = liftIO $ BS.unpack <$> makePassword (BS.pack password) 17

updateUserTimestamp :: User -> AppContextM User
updateUserTimestamp user = do
  now <- liftIO getCurrentTime
  return $ user & updatedAt .~ now

sendAnalyticsEmailIfEnabled user = do
  serverConfig <- asks _appContextServerConfig
  when (serverConfig ^. analytics . enabled) (sendRegistrationCreatedAnalyticsMail (toDTO user))

checkIfRegistrationIsEnabled =
  checkIfAppFeatureIsEnabled "Registration" (authentication . internal . registration . enabled)
