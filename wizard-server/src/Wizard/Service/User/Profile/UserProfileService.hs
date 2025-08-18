module Wizard.Service.User.Profile.UserProfileService where

import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.SensitiveData
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.DAO.User.UserSubmissionPropDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.User.User
import Wizard.Model.User.UserSubmissionPropEM ()
import Wizard.Model.User.UserSubmissionPropList
import Wizard.Service.User.Profile.UserProfileMapper
import Wizard.Service.User.Profile.UserProfileValidation
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import Wizard.Service.User.UserValidation
import WizardLib.Public.Api.Resource.User.UserLocaleDTO

getUserProfile :: AppContextM UserDTO
getUserProfile = getCurrentUser

modifyUserProfile :: UserProfileChangeDTO -> AppContextM UserDTO
modifyUserProfile reqDto = do
  currentUser <- getCurrentUser
  user <- findUserByUuid currentUser.uuid
  validateUserChangedEmailUniqueness reqDto.email user.email
  now <- liftIO getCurrentTime
  let updatedUser = fromUserProfileChangeDTO user reqDto now
  updateUserByUuid updatedUser
  return . toDTO $ updatedUser

changeUserProfilePassword :: U.UUID -> UserPasswordDTO -> AppContextM ()
changeUserProfilePassword userUuid reqDto = do
  user <- findUserByUuid userUuid
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
  updateUserLocaleByUuid user.uuid reqDto.lId now
  return reqDto
