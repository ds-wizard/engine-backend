module Wizard.Service.User.UserProfileService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Util.List (groupBy)
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Model.User.UserEM ()
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.User.UserProfileMapper
import Wizard.Service.User.UserService
import Wizard.Service.User.UserValidation

getUserProfile :: U.UUID -> AppContextM UserProfileDTO
getUserProfile userUuid = do
  appConfig <- getAppConfig
  userDecrypted <- getDecryptedUser
  let userProps = computeUserPropsForProfile appConfig userDecrypted
  return $ toUserProfileDTO userDecrypted userProps
  where
    getDecryptedUser :: AppContextM User
    getDecryptedUser = do
      serverConfig <- asks serverConfig
      user <- findUserByUuid userUuid
      return $ process serverConfig.general.secret user
    computeUserPropsForProfile :: AppConfig -> User -> [UserSubmissionPropsDTO]
    computeUserPropsForProfile appConfig user =
      let userPropsFromService = fmap fromService appConfig.submission.services
          userPropsFromUser = fmap fromUserSubmissionProps user.submissionProps
          groupedProps :: [[UserSubmissionPropsDTO]]
          groupedProps = groupBy groupFn (userPropsFromService ++ userPropsFromUser)
          groupFn :: UserSubmissionPropsDTO -> UserSubmissionPropsDTO -> Bool
          groupFn p1 p2 = p1.sId == p2.sId
          merge :: [UserSubmissionPropsDTO] -> UserSubmissionPropsDTO
          merge [p] = p
          merge [pFromService, pFromUser] =
            pFromService {values = M.mapWithKey (mergeFn pFromUser) pFromService.values}
          mergeFn :: UserSubmissionPropsDTO -> String -> String -> String
          mergeFn pFromUser k v = fromMaybe "" (M.lookup k pFromUser.values)
       in fmap merge groupedProps

modifyUserProfile :: U.UUID -> UserProfileChangeDTO -> AppContextM UserProfileDTO
modifyUserProfile userUuid reqDto = do
  serverConfig <- asks serverConfig
  user <- findUserByUuid userUuid
  validateUserChangedEmailUniqueness reqDto.email user.email
  updatedUser <- updateUserTimestamp $ fromUserProfileChangeDTO reqDto user
  let encryptedUpdatedUser = process serverConfig.general.secret updatedUser
  updateUserByUuid encryptedUpdatedUser
  getUserProfile userUuid

changeUserProfilePassword :: U.UUID -> UserPasswordDTO -> AppContextM ()
changeUserProfilePassword userUuid reqDto = do
  user <- findUserByUuid userUuid
  passwordHash <- generatePasswordHash reqDto.password
  now <- liftIO getCurrentTime
  updateUserPasswordByUuid userUuid passwordHash now
  return ()
