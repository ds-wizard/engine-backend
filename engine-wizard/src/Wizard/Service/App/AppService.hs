module Wizard.Service.App.AppService where

import Control.Lens ((.~), (^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.App.AppAdminCreateDTO
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.App.App
import Wizard.Model.App.ImportDefaultDataCommand
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigDM
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Model.User.User
import Wizard.Model.User.UserDM
import Wizard.Service.App.AppMapper
import Wizard.Service.App.AppValidation
import Wizard.Service.Limit.AppLimitService
import Wizard.Service.PersistentCommand.PersistentCommandMapper
import qualified Wizard.Service.User.UserMapper as U_Mapper
import Wizard.Service.User.UserService

getApps :: Maybe String -> AppContextM [AppDTO]
getApps mAppId = do
  case mAppId of
    Nothing -> return []
    Just appId -> do
      apps <- findAppsByAppId appId
      return . fmap toDTO $ apps

createApp :: AppCreateDTO -> AppContextM AppDTO
createApp reqDto = do
  runInTransaction $ do
    validateAppCreateDTO reqDto
    aUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    serverConfig <- asks _appContextServerConfig
    let cloudDomain = fromMaybe "" (serverConfig ^. cloud . domain)
    let app = fromCreateDTO reqDto aUuid cloudDomain now
    insertApp app
    userUuid <- liftIO generateUuid
    user <-
      createUserByAdminWithUuid (U_Mapper.fromAppCreateToUserCreateDTO reqDto) userUuid (app ^. uuid) (app ^. clientUrl)
    createAppConfig aUuid now
    createAppLimit aUuid now
    createSeederPersistentCommand aUuid (user ^. uuid) now
    return . toDTO $ app

createAdminApp :: AppAdminCreateDTO -> AppContextM App
createAdminApp reqDto = do
  runInTransaction $ do
    aUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let app = fromAdminCreate reqDto aUuid now
    insertApp app
    user <- createUser' aUuid now
    createAppConfig aUuid now
    createAppLimit aUuid now
    createSeederPersistentCommand aUuid (user ^. uuid) now
    return app

-- --------------------------------
-- PRIVATE
-- --------------------------------
createAppConfig :: U.UUID -> UTCTime -> AppContextM AppConfig
createAppConfig aUuid now = do
  runInTransaction $ do
    let appConfig = (uuid .~ aUuid) . (createdAt .~ now) . (updatedAt .~ now) $ defaultAppConfig
    insertAppConfig appConfig
    return appConfig

createUser' :: U.UUID -> UTCTime -> AppContextM User
createUser' aUuid now =
  runInTransaction $ do
    uUuid <- liftIO generateUuid
    let user =
          (uuid .~ uUuid) . (appUuid .~ aUuid) . (lastVisitedAt .~ now) . (createdAt .~ now) . (updatedAt .~ now) $
          defaultUser
    insertUser user
    return user

createSeederPersistentCommand :: U.UUID -> U.UUID -> UTCTime -> AppContextM PersistentCommand
createSeederPersistentCommand aUuid createdBy now =
  runInTransaction $ do
    pUuid <- liftIO generateUuid
    let command =
          toPersistentCommand
            pUuid
            "DataSeeder"
            "importDefaultData"
            (BSL.unpack . encode $ ImportDefaultDataCommand aUuid)
            1
            False
            aUuid
            createdBy
            now
    insertPersistentCommand command
    return command
