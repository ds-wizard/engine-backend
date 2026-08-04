module Wizard.Service.User.Profile.UserProfileService where

import Control.Monad (forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Model.Error.Error
import Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.DAO.User.UserSubmissionPropDAO
import Wizard.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Model.User.UserSubmissionPropEM ()
import Wizard.Model.User.UserSubmissionPropList
import Wizard.Model.UserEmailLink.UserEmailLinkType
import Wizard.Service.Mail.Mailer
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.User.Profile.UserProfileMapper
import Wizard.Service.User.Profile.UserProfileValidation
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import Wizard.Service.User.UserValidation
import Wizard.Service.UserEmailLink.UserEmailLinkService
import WizardLib.Public.Api.Resource.User.UserLocaleDTO
import WizardLib.Public.Localization.Messages.Public
import WizardLib.Public.Model.User.RolePermission

getUserProfile :: AppContextM UserDTO
getUserProfile = getCurrentUser

modifyUserProfile :: UserProfileChangeDTO -> AppContextM UserDTO
modifyUserProfile reqDto = do
  currentUser <- getCurrentUser
  user <- findUserByUuid currentUser.uuid
  let newEmail = toLower <$> reqDto.email
  let emailChanged = newEmail /= user.email
  let revertPending = not emailChanged && maybe False (/= user.email) user.emailPending
  when emailChanged $ validateUserChangedEmailUniqueness reqDto.email user.email
  now <- liftIO getCurrentTime
  let updatedUser = fromUserProfileChangeDTO reqDto user revertPending now
  updateUserByUuid updatedUser
  when revertPending $ do
    mUserEmailLink :: Maybe (UserEmailLink U.UUID UserEmailLinkType) <-
      findUserEmailLinkByIdentityAndType' (U.toString currentUser.uuid) EmailChangeUserEmailLinkType
    forM_ mUserEmailLink $ \ak -> deleteUserEmailLinkByHash ak.hash
  when emailChanged $ do
    tenantUuid <- asks currentTenantUuid
    userEmailLink <- createUserEmailLink currentUser.uuid EmailChangeUserEmailLinkType tenantUuid
    sendEmailChangeMail updatedUser userEmailLink.hash newEmail
  return . toDTO $ updatedUser

changeUserProfilePassword :: U.UUID -> UserPasswordDTO -> AppContextM ()
changeUserProfilePassword userUuid reqDto = do
  tcAuthentication <- getCurrentTenantConfigAuthentication
  user <- findUserByUuid userUuid
  when (not tcAuthentication.internal.nonAdminLoginEnabled && notElem _USERS_MANAGE_ROLE_PERMISSION user.role.permissions) $
    throwError . UserError $
      _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
  passwordHash <- generatePasswordHash reqDto.password
  now <- liftIO getCurrentTime
  updateUserPasswordByUuid userUuid passwordHash now
  return ()

getUserProfileSubmissionProps :: U.UUID -> AppContextM [UserSubmissionPropList]
getUserProfileSubmissionProps userUuid = do
  serverConfig <- asks serverConfig
  submissionProps <- findUserSubmissionPropsList userUuid
  return . fmap (process serverConfig.general.secret) $ submissionProps

modifyUserProfileSubmissionProps :: [UserSubmissionPropList] -> AppContextM [UserSubmissionPropList]
modifyUserProfileSubmissionProps reqDto = do
  currentUser <- getCurrentUser
  tenantUuid <- asks currentTenantUuid
  serverConfig <- asks serverConfig
  submissionPropsEncrypted <- findUserSubmissionProps currentUser.uuid
  let submissionProps = fmap (process serverConfig.general.secret) submissionPropsEncrypted
  now <- liftIO getCurrentTime
  let submissionPropsUpdated = fromUserSubmissionPropsDTO currentUser.uuid tenantUuid submissionProps reqDto now
  let submissionPropsUpdatedEncrypted = fmap (process serverConfig.general.secret) submissionPropsUpdated
  traverse_ insertOrUpdateUserSubmissionProp submissionPropsUpdatedEncrypted
  deleteUserSubmissionPropsExcept currentUser.uuid (map (.sId) reqDto)
  return reqDto

getLocale :: AppContextM UserLocaleDTO
getLocale = do
  user <- getCurrentUser
  return . UserLocaleDTO $ user.locale

modifyLocale :: UserLocaleDTO -> AppContextM UserLocaleDTO
modifyLocale reqDto = do
  validateLocale reqDto
  user <- getCurrentUser
  now <- liftIO getCurrentTime
  updateUserLocaleByUuid user.uuid reqDto.uuid now
  return reqDto
