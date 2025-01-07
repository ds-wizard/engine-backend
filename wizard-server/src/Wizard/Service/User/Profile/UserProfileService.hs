module Wizard.Service.User.Profile.UserProfileService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.SensitiveData
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.User.Profile.UserProfileMapper
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import Wizard.Service.User.UserUtil
import Wizard.Service.User.UserValidation

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

getUserProfileSubmissionProps :: U.UUID -> AppContextM [UserSubmissionPropsDTO]
getUserProfileSubmissionProps userUuid = do
  userDecrypted <- getDecryptedUser userUuid
  tenantConfig <- getCurrentTenantConfig
  return . fmap (mapFn userDecrypted) $ tenantConfig.submission.services
  where
    mapFn :: User -> TenantConfigSubmissionService -> UserSubmissionPropsDTO
    mapFn userDecrypted service =
      UserSubmissionPropsDTO
        { sId = service.sId
        , name = service.name
        , values =
            let defaultProps = M.fromList . fmap (\v -> (v, "")) $ service.props
                userProps = maybe M.empty ((.values)) . L.find (\s -> s.sId == service.sId) $ userDecrypted.submissionProps
             in M.union userProps defaultProps
        }

modifyUserProfileSubmissionProps :: [UserSubmissionPropsDTO] -> AppContextM [UserSubmissionPropsDTO]
modifyUserProfileSubmissionProps reqDto = do
  currentUser <- getCurrentUser
  serverConfig <- asks serverConfig
  user <- findUserByUuid currentUser.uuid
  now <- liftIO getCurrentTime
  let updatedUser = fromUserSubmissionPropsDTO user reqDto now
  let encryptedUpdatedUser = process serverConfig.general.secret updatedUser
  updateUserByUuid encryptedUpdatedUser
  return reqDto
