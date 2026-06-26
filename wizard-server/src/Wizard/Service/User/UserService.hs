module Wizard.Service.User.UserService where

import Control.Monad (unless, void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Crypto.PasswordStore as PasswordStore
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.SimpleFeature
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Crypto (generateRandomString)
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink
import Wizard.Api.Resource.Auth.AuthConsentDTO
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Wizard.Localization.Messages.Internal
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.UserSubmissionPropEM ()
import Wizard.Model.UserEmailLink.UserEmailLinkType
import Wizard.Service.Common
import Wizard.Service.Mail.Mailer
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import Wizard.Service.Tenant.TenantHelper
import Wizard.Service.User.UserAudit
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserValidation
import Wizard.Service.UserEmailLink.UserEmailLinkService
import Wizard.Service.UserToken.Login.LoginService
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Database.DAO.User.RoleDAO
import WizardLib.Public.Database.DAO.User.UserOpenIdIdentityDAO
import WizardLib.Public.Model.OpenId.OpenIdClient
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateUserCommand
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Model.User.UserSuggestion
import qualified WizardLib.Public.Service.User.UserOpenIdIdentityMapper as UserOpenIdIdentityMapper

getUsersPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page UserDTO)
getUsersPage mQuery mRole pageable sort = do
  checkPermission _USERS_MANAGE_ROLE_PERMISSION
  userPage <- findUsersPage mQuery mRole pageable sort
  return . fmap toDTO $ userPage

getUserSuggestionsPage :: Maybe String -> Maybe [String] -> Maybe [String] -> Pageable -> [Sort] -> AppContextM (Page UserSuggestion)
getUserSuggestionsPage = findUserSuggestionsPage

registerOrCreateUserByAdmin :: UserCreateDTO -> AppContextM UserDTO
registerOrCreateUserByAdmin reqDto =
  runInTransaction $ do
    isAdmin <- isCurrentUserAdmin
    if isAdmin
      then createUserByAdmin reqDto
      else registerUser reqDto

createUserByAdmin :: UserCreateDTO -> AppContextM UserDTO
createUserByAdmin reqDto =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    checkIfAdminIsDisabled
    uUuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    clientUrl <- getClientUrl
    createUserByAdminWithUuid reqDto uUuid tenantUuid clientUrl False

createUserByAdminWithUuid :: UserCreateDTO -> U.UUID -> U.UUID -> String -> Bool -> AppContextM UserDTO
createUserByAdminWithUuid reqDto uUuid tenantUuid clientUrl shouldSendRegistrationEmail =
  runInTransaction $ do
    uPasswordHash <- generatePasswordHash reqDto.password
    tcAuthentication <- getCurrentTenantConfigAuthentication
    let role = fromMaybe tcAuthentication.defaultRoleUuid reqDto.roleUuid
    uRole <- getRoleForUserInTenant tenantUuid role
    userDto <- createUser reqDto uUuid uPasswordHash role uRole.permissions uRole.name tenantUuid clientUrl shouldSendRegistrationEmail
    auditUserCreateByAdmin userDto
    return userDto

registerUser :: UserCreateDTO -> AppContextM UserDTO
registerUser reqDto =
  runInTransaction $ do
    checkIfAdminIsDisabled
    checkIfRegistrationIsEnabled
    uUuid <- liftIO generateUuid
    uPasswordHash <- generatePasswordHash reqDto.password
    tcAuthentication <- getCurrentTenantConfigAuthentication
    let role = tcAuthentication.defaultRoleUuid
    uRole <- getRoleForUser role
    clientUrl <- getClientUrl
    tenantUuid <- asks currentTenantUuid
    mExistingUser <- findUserByEmailAndTenantUuid' (toLower reqDto.email) tenantUuid
    case mExistingUser of
      Just _ -> do
        now <- liftIO getCurrentTime
        let fakeUser = fromUserCreateDTO reqDto uUuid uPasswordHash role uRole.permissions uRole.name tenantUuid now True
        return $ toDTO fakeUser
      Nothing -> createUser reqDto uUuid uPasswordHash role uRole.permissions uRole.name tenantUuid clientUrl True

createUser :: UserCreateDTO -> U.UUID -> String -> U.UUID -> [String] -> String -> U.UUID -> String -> Bool -> AppContextM UserDTO
createUser reqDto uUuid uPasswordHash role uPermissions uRoleName tenantUuid clientUrl shouldSendRegistrationEmail =
  runInTransaction $ do
    checkUserLimit
    checkActiveUserLimit
    validateUserEmailUniqueness reqDto.email tenantUuid
    now <- liftIO getCurrentTime
    let user = fromUserCreateDTO reqDto uUuid uPasswordHash role uPermissions uRoleName tenantUuid now shouldSendRegistrationEmail
    insertUser user
    userEmailLink <- createUserEmailLink uUuid RegistrationUserEmailLinkType tenantUuid
    when
      shouldSendRegistrationEmail
      ( catchError
          (sendRegistrationConfirmationMail user userEmailLink.hash clientUrl)
          (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT)
      )
    sendAnalyticsEmailIfEnabled user
    return $ toDTO user

createUserFromOpenIdLogin
  :: OpenIdClient
  -> String
  -> String
  -> String
  -> String
  -> Maybe String
  -> Maybe U.UUID
  -> Bool
  -> AppContextM User
createUserFromOpenIdLogin openIdClient externalId firstName lastName email mImageUrl mUserUuid active =
  runInTransaction $ do
    checkUserLimit
    checkActiveUserLimit
    now <- liftIO getCurrentTime
    tenantUuid <- asks currentTenantUuid
    uUuid <-
      case mUserUuid of
        Just userUuid -> return userUuid
        Nothing -> liftIO generateUuid
    password <- liftIO $ generateRandomString 40
    uPasswordHash <- generatePasswordHash password
    tcAuthentication <- getCurrentTenantConfigAuthentication
    let role = tcAuthentication.defaultRoleUuid
    uRole <- getRoleForUser role
    let user =
          fromUserExternalDTO
            uUuid
            firstName
            lastName
            email
            uPasswordHash
            role
            uRole.permissions
            uRole.name
            active
            mImageUrl
            tenantUuid
            now
    insertUser user
    identityUuid <- liftIO generateUuid
    let identity = UserOpenIdIdentityMapper.fromCreate identityUuid externalId Nothing user.uuid openIdClient.uuid openIdClient.tenantUuid now
    _ <- insertUserOpenIdIdentity identity
    sendAnalyticsEmailIfEnabled user
    return user

createOrUpdateUserFromCommand :: CreateOrUpdateUserCommand -> AppContextM User
createOrUpdateUserFromCommand command =
  runInTransaction $ do
    mUserFromDb <- findUserByUuidSystem' command.uuid command.tenantUuid
    now <- liftIO getCurrentTime
    case mUserFromDb of
      Just userFromDb -> do
        (uPermissions, uRoleName) <-
          if userFromDb.role.uuid == command.roleUuid
            then return (userFromDb.role.permissions, userFromDb.role.name)
            else do
              uRole <- getRoleForUser command.roleUuid
              return (uRole.permissions, uRole.name)
        let updatedUser = fromCommandChangeDTO userFromDb command uPermissions uRoleName now
        updateUserByUuid updatedUser
        return updatedUser
      Nothing -> do
        checkUserLimit
        checkActiveUserLimit
        uRole <- getRoleForUser command.roleUuid
        let user = fromCommandCreateDTO command uRole.permissions uRole.name now
        insertUser user
        return user

getUserById :: U.UUID -> AppContextM UserDTO
getUserById userUuid = do
  user <- findUserByUuid userUuid
  return $ toDTO user

getUserDetailById :: U.UUID -> AppContextM UserDTO
getUserDetailById userUuid = do
  checkPermission _USERS_MANAGE_ROLE_PERMISSION
  getUserById userUuid

modifyUser :: U.UUID -> UserChangeDTO -> AppContextM UserDTO
modifyUser userUuid reqDto =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    user <- findUserByUuid userUuid
    when (reqDto.active && not user.active) checkActiveUserLimit
    validateUserChangedEmailUniqueness reqDto.email user.email
    (newPermissions, newRoleName) <-
      if reqDto.roleUuid /= user.role.uuid
        then do
          newRole <- getRoleForUser reqDto.roleUuid
          return (newRole.permissions, newRole.name)
        else return (user.role.permissions, user.role.name)
    updatedUser <- updateUserTimestamp $ fromUserChangeDTO reqDto user newPermissions newRoleName
    updateUserByUuid updatedUser
    return . toDTO $ updatedUser

changeUserPasswordByAdminOrHash :: U.UUID -> UserPasswordDTO -> Maybe String -> AppContextM ()
changeUserPasswordByAdminOrHash userUuid reqDto mHash =
  runInTransaction $ do
    isAdmin <- isCurrentUserAdmin
    if isAdmin
      then changeUserPasswordByAdmin userUuid reqDto
      else do
        let hash = fromMaybe (U.toString U.nil) mHash
        changeUserPasswordByHash userUuid hash reqDto

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
    userEmailLink <- findUserEmailLinkByHash hash :: AppContextM (UserEmailLink U.UUID UserEmailLinkType)
    validateUserEmailLinkNotExpired userEmailLink
    user <- findUserByUuid userEmailLink.identity
    passwordHash <- generatePasswordHash userPasswordDto.password
    now <- liftIO getCurrentTime
    updateUserPasswordByUuid userUuid passwordHash now
    deleteUserEmailLinkByHash userEmailLink.hash
    return ()

resetUserPassword :: UserEmailLinkDTO UserEmailLinkType -> AppContextM ()
resetUserPassword reqDto =
  runInTransaction $ do
    mUser <- findUserByEmail' (toLower reqDto.email)
    case mUser of
      Just user -> do
        tcAuthentication <- getCurrentTenantConfigAuthentication
        unless (not tcAuthentication.internal.nonAdminLoginEnabled && notElem _USERS_MANAGE_ROLE_PERMISSION user.role.permissions) $ do
          tenantUuid <- asks currentTenantUuid
          userEmailLink <- createUserEmailLink user.uuid ForgottenPasswordUserEmailLinkType tenantUuid
          catchError
            (sendResetPasswordMail (toDTO user) userEmailLink.hash)
            (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT)
      Nothing -> return ()

changeUserState :: String -> Bool -> AppContextM ()
changeUserState hash active =
  runInTransaction $ do
    checkActiveUserLimit
    userEmailLink <- findUserEmailLinkByHash hash :: AppContextM (UserEmailLink U.UUID UserEmailLinkType)
    validateUserEmailLinkNotExpired userEmailLink
    user <- findUserByUuid userEmailLink.identity
    now <- liftIO getCurrentTime
    let baseUser :: User
        baseUser = user {active = active, updatedAt = now}
    let updatedUser :: User
        updatedUser =
          case (userEmailLink.aType, user.emailPending) of
            (RegistrationUserEmailLinkType, Just pendingEmail) ->
              baseUser
                { email = pendingEmail
                , emailVerifiedAt = Just now
                , emailPending = Nothing
                }
            _ -> baseUser
    updateUserByUuid updatedUser
    void $ deleteUserEmailLinkByHash userEmailLink.hash

confirmEmailChange :: String -> AppContextM ()
confirmEmailChange hash =
  runInTransaction $ do
    userEmailLink <- findUserEmailLinkByHashAndType hash EmailChangeUserEmailLinkType :: AppContextM (UserEmailLink U.UUID UserEmailLinkType)
    validateUserEmailLinkNotExpired userEmailLink
    user <- findUserByUuid userEmailLink.identity
    now <- liftIO getCurrentTime
    case user.emailPending of
      Just newEmail -> do
        validateUserEmailUniqueness newEmail user.tenantUuid
        let updatedUser :: User
            updatedUser =
              user
                { email = newEmail
                , emailPending = Nothing
                , emailVerifiedAt = Just now
                , updatedAt = now
                }
        updateUserByUuid updatedUser
        void $ deleteUserEmailLinkByHash userEmailLink.hash
      Nothing -> void $ deleteUserEmailLinkByHash userEmailLink.hash

confirmConsents :: AuthConsentDTO -> Maybe String -> AppContextM UserTokenDTO
confirmConsents reqDto mUserAgent = do
  userEmailLink <- findUserEmailLinkByHash reqDto.hash :: AppContextM (UserEmailLink U.UUID UserEmailLinkType)
  validateUserEmailLinkNotExpired userEmailLink
  user <- findUserByUuid userEmailLink.identity
  changeUserState reqDto.hash True
  createLoginToken user mUserAgent reqDto.sessionState

deleteUser :: U.UUID -> AppContextM ()
deleteUser userUuid =
  runInTransaction $ do
    checkPermission _USERS_MANAGE_ROLE_PERMISSION
    _ <- findUserByUuid userUuid
    void $ deleteUserByUuid userUuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
getRoleForUser :: U.UUID -> AppContextM Role
getRoleForUser roleUuid = do
  tenantUuid <- asks currentTenantUuid
  getRoleForUserInTenant tenantUuid roleUuid

getRoleForUserInTenant :: U.UUID -> U.UUID -> AppContextM Role
getRoleForUserInTenant tenantUuid roleUuid = findRoleByUuidAndTenant roleUuid tenantUuid

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
  when serverConfig.analyticalMails.enabled (sendRegistrationCreatedAnalyticsMail user)

checkIfRegistrationIsEnabled = checkIfTenantFeatureIsEnabled "Registration" getCurrentTenantConfigAuthentication (.internal.registration.enabled)

checkIfAdminIsDisabled =
  checkIfServerFeatureIsEnabled "User Management Endpoints" (\s -> not s.admin.enabled)
