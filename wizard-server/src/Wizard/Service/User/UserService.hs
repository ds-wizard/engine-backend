module Wizard.Service.User.UserService where

import Control.Monad (forM_, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Crypto.PasswordStore as PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyDTO
import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.SimpleFeature
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Crypto (generateRandomString)
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Api.Resource.Auth.AuthConsentDTO
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Mapping.ActionKey.ActionKeyType ()
import Wizard.Localization.Messages.Internal
import Wizard.Model.ActionKey.ActionKeyType
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.UserEM ()
import Wizard.S3.Document.DocumentS3
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Common
import Wizard.Service.Mail.Mailer
import Wizard.Service.Questionnaire.QuestionnaireService
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import Wizard.Service.Tenant.TenantHelper
import Wizard.Service.User.UserAudit
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserValidation
import Wizard.Service.UserToken.Login.LoginService
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Localization.Messages.Public
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateUserCommand
import WizardLib.Public.Service.UserToken.UserTokenService

getUsersPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page UserDTO)
getUsersPage mQuery mRole pageable sort = do
  checkPermission _UM_PERM
  userPage <- findUsersPage mQuery mRole pageable sort
  return . fmap toDTO $ userPage

getUserSuggestionsPage :: Maybe String -> Maybe [String] -> Maybe [String] -> Pageable -> [Sort] -> AppContextM (Page UserSuggestionDTO)
getUserSuggestionsPage mQuery mSelectUuids mExcludeUuids pageable sort = do
  suggestionPage <- findUserSuggestionsPage mQuery mSelectUuids mExcludeUuids pageable sort
  return . fmap toSuggestionDTO $ suggestionPage

createUserByAdmin :: UserCreateDTO -> AppContextM UserDTO
createUserByAdmin reqDto =
  runInTransaction $ do
    checkPermission _UM_PERM
    uUuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    clientUrl <- getClientUrl
    createUserByAdminWithUuid reqDto uUuid tenantUuid clientUrl False

createUserByAdminWithUuid :: UserCreateDTO -> U.UUID -> U.UUID -> String -> Bool -> AppContextM UserDTO
createUserByAdminWithUuid reqDto uUuid tenantUuid clientUrl shouldSendRegistrationEmail =
  runInTransaction $ do
    uPasswordHash <- generatePasswordHash reqDto.password
    serverConfig <- asks serverConfig
    tenantConfig <- getCurrentTenantConfig
    let uRole = fromMaybe tenantConfig.authentication.defaultRole reqDto.uRole
    let uPermissions = getPermissionForRole serverConfig uRole
    userDto <- createUser reqDto uUuid uPasswordHash uRole uPermissions tenantUuid clientUrl shouldSendRegistrationEmail
    auditUserCreateByAdmin userDto
    return userDto

registerUser :: UserCreateDTO -> AppContextM UserDTO
registerUser reqDto =
  runInTransaction $ do
    checkIfRegistrationIsEnabled
    uUuid <- liftIO generateUuid
    uPasswordHash <- generatePasswordHash reqDto.password
    serverConfig <- asks serverConfig
    tenantConfig <- getCurrentTenantConfig
    let uRole = tenantConfig.authentication.defaultRole
    let uPermissions = getPermissionForRole serverConfig uRole
    clientUrl <- getClientUrl
    tenantUuid <- asks currentTenantUuid
    createUser reqDto uUuid uPasswordHash uRole uPermissions tenantUuid clientUrl True

createUser :: UserCreateDTO -> U.UUID -> String -> String -> [String] -> U.UUID -> String -> Bool -> AppContextM UserDTO
createUser reqDto uUuid uPasswordHash uRole uPermissions tenantUuid clientUrl shouldSendRegistrationEmail =
  runInTransaction $ do
    checkUserLimit
    checkActiveUserLimit
    validateUserEmailUniqueness reqDto.email tenantUuid
    now <- liftIO getCurrentTime
    let user = fromUserCreateDTO reqDto uUuid uPasswordHash uRole uPermissions tenantUuid now shouldSendRegistrationEmail
    insertUser user
    actionKey <- createActionKey uUuid RegistrationActionKey tenantUuid
    when
      shouldSendRegistrationEmail
      ( catchError
          (sendRegistrationConfirmationMail (toDTO user) actionKey.hash clientUrl)
          (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT)
      )
    sendAnalyticsEmailIfEnabled user
    return $ toDTO user

createUserFromExternalService :: Maybe User -> String -> String -> String -> String -> Maybe String -> Maybe U.UUID -> Bool -> AppContextM User
createUserFromExternalService mUserFromDb serviceId firstName lastName email mImageUrl mUserUuid active =
  runInTransaction $ do
    now <- liftIO getCurrentTime
    tenantUuid <- asks currentTenantUuid
    case mUserFromDb of
      Just user ->
        if user.active
          then do
            let updatedUser = fromUpdateUserExternalDTO user firstName lastName mImageUrl serviceId now
            updateUserByUuid updatedUser
            return updatedUser
          else throwError $ UserError _ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED
      Nothing -> do
        checkUserLimit
        checkActiveUserLimit
        serverConfig <- asks serverConfig
        uUuid <-
          case mUserUuid of
            Just userUuid -> return userUuid
            Nothing -> liftIO generateUuid
        password <- liftIO $ generateRandomString 40
        uPasswordHash <- generatePasswordHash password
        tenantConfig <- getCurrentTenantConfig
        let uRole = tenantConfig.authentication.defaultRole
        let uPerms = getPermissionForRole serverConfig uRole
        let user =
              fromUserExternalDTO
                uUuid
                firstName
                lastName
                email
                uPasswordHash
                [serviceId]
                uRole
                uPerms
                active
                mImageUrl
                tenantUuid
                now
        insertUser user
        sendAnalyticsEmailIfEnabled user
        return user

createOrUpdateUserFromCommand :: CreateOrUpdateUserCommand -> AppContextM User
createOrUpdateUserFromCommand command =
  runInTransaction $ do
    mUserFromDb <- findUserByUuidSystem' command.uuid
    serverConfig <- asks serverConfig
    now <- liftIO getCurrentTime
    case mUserFromDb of
      Just userFromDb -> do
        let uPermissions =
              if userFromDb.uRole == command.uRole
                then userFromDb.permissions
                else getPermissionForRole serverConfig command.uRole
        let updatedUser = fromCommandChangeDTO userFromDb command uPermissions now
        updateUserByUuid updatedUser
        return updatedUser
      Nothing -> do
        checkUserLimit
        checkActiveUserLimit
        let uPerms = getPermissionForRole serverConfig command.uRole
        let user = fromCommandCreateDTO command uPerms now
        insertUser user
        return user

getUserById :: U.UUID -> AppContextM UserDTO
getUserById userUuid = do
  user <- findUserByUuid userUuid
  return $ toDTO user

getUserDetailById :: U.UUID -> AppContextM UserDTO
getUserDetailById userUuid = do
  checkPermission _UM_PERM
  getUserById userUuid

modifyUser :: U.UUID -> UserChangeDTO -> AppContextM UserDTO
modifyUser userUuid reqDto =
  runInTransaction $ do
    checkPermission _UM_PERM
    user <- findUserByUuid userUuid
    when (reqDto.active && not user.active) checkActiveUserLimit
    validateUserChangedEmailUniqueness reqDto.email user.email
    serverConfig <- asks serverConfig
    updatedUser <- updateUserTimestamp $ fromUserChangeDTO reqDto user (getPermissions serverConfig reqDto user)
    updateUserByUuid updatedUser
    return . toDTO $ updatedUser
  where
    getPermissions serverConfig reqDto oldUser =
      if reqDto.uRole /= oldUser.uRole
        then getPermissionForRole serverConfig reqDto.uRole
        else oldUser.permissions

changeUserPasswordByAdmin :: U.UUID -> UserPasswordDTO -> AppContextM ()
changeUserPasswordByAdmin userUuid reqDto =
  runInTransaction $ do
    user <- findUserByUuid userUuid
    passwordHash <- generatePasswordHash reqDto.password
    now <- liftIO getCurrentTime
    updateUserPasswordByUuid userUuid passwordHash now
    return ()

changeUserPasswordByHash :: U.UUID -> String -> UserPasswordDTO -> AppContextM ()
changeUserPasswordByHash userUuid hash userPasswordDto =
  runInTransaction $ do
    actionKey <- findActionKeyByHash hash :: AppContextM (ActionKey U.UUID ActionKeyType)
    user <- findUserByUuid actionKey.identity
    passwordHash <- generatePasswordHash userPasswordDto.password
    now <- liftIO getCurrentTime
    updateUserPasswordByUuid userUuid passwordHash now
    deleteActionKeyByHash actionKey.hash
    return ()

resetUserPassword :: ActionKeyDTO ActionKeyType -> AppContextM ()
resetUserPassword reqDto =
  runInTransaction $ do
    user <- findUserByEmail reqDto.email
    tenantUuid <- asks currentTenantUuid
    actionKey <- createActionKey user.uuid ForgottenPasswordActionKey tenantUuid
    catchError
      (sendResetPasswordMail (toDTO user) actionKey.hash)
      (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT)

changeUserState :: String -> Bool -> AppContextM ()
changeUserState hash active =
  runInTransaction $ do
    checkActiveUserLimit
    actionKey <- findActionKeyByHash hash :: AppContextM (ActionKey U.UUID ActionKeyType)
    user <- findUserByUuid actionKey.identity
    updatedUser <- updateUserTimestamp $ user {active = active}
    updateUserByUuid updatedUser
    deleteActionKeyByHash actionKey.hash
    return ()

confirmConsents :: AuthConsentDTO -> Maybe String -> AppContextM UserTokenDTO
confirmConsents reqDto mUserAgent = do
  actionKey <- findActionKeyByHash reqDto.hash :: AppContextM (ActionKey U.UUID ActionKeyType)
  user <- findUserByUuid actionKey.identity
  changeUserState reqDto.hash True
  createLoginToken user mUserAgent reqDto.sessionState

deleteUser :: U.UUID -> AppContextM ()
deleteUser userUuid =
  runInTransaction $ do
    checkPermission _UM_PERM
    _ <- findUserByUuid userUuid
    clearBranchCreatedBy userUuid
    removeOwnerFromQuestionnaire userUuid
    clearQuestionnaireCreatedBy userUuid
    deletePersistentCommandByCreatedBy userUuid
    documents <- findDocumentsFiltered [("created_by", U.toString userUuid)]
    forM_
      documents
      ( \d -> do
          deleteDocumentsFiltered [("uuid", U.toString d.uuid)]
          removeDocumentContent d.uuid
      )
    deleteTokenByUserUuid userUuid
    deleteUserByUuid userUuid
    return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPermissionForRole :: ServerConfig -> String -> [String]
getPermissionForRole config role
  | role == _USER_ROLE_ADMIN = config.roles.admin
  | role == _USER_ROLE_DATA_STEWARD = config.roles.dataSteward
  | role == _USER_ROLE_RESEARCHER = config.roles.researcher
  | otherwise = []

generatePasswordHash :: String -> AppContextM String
generatePasswordHash password = do
  hash <- liftIO $ BS.unpack <$> PasswordStore.makePasswordWith PasswordStore.pbkdf2 (BS.pack password) 17
  return $ "pbkdf2:" ++ hash

updateUserTimestamp :: User -> AppContextM User
updateUserTimestamp user = do
  now <- liftIO getCurrentTime
  return $ user {updatedAt = now}

sendAnalyticsEmailIfEnabled user = do
  serverConfig <- asks serverConfig
  when serverConfig.analytics.enabled (sendRegistrationCreatedAnalyticsMail (toDTO user))

checkIfRegistrationIsEnabled =
  checkIfTenantFeatureIsEnabled "Registration" (\c -> c.authentication.internal.registration.enabled)
