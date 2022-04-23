module Wizard.Service.App.AppService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Util.Crypto
import Shared.Util.Uuid
import Wizard.Api.Resource.App.AppChangeDTO
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppDetailDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Database.DAO.Plan.AppPlanDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigDM
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.App.ImportDefaultDataCommand
import Wizard.Model.PersistentCommand.PersistentCommand
import Wizard.Model.User.User
import Wizard.Service.Acl.AclService
import Wizard.Service.App.AppMapper
import Wizard.Service.App.AppUtil
import Wizard.Service.App.AppValidation
import Wizard.Service.Limit.AppLimitService
import qualified Wizard.Service.PersistentCommand.PersistentCommandMapper as PCM
import Wizard.Service.Usage.UsageService
import qualified Wizard.Service.User.UserMapper as U_Mapper
import Wizard.Service.User.UserService

getAppsPage :: Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page AppDTO)
getAppsPage mQuery mEnabled pageable sort = do
  checkPermission _APP_PERM
  apps <- findAppsPage mQuery mEnabled pageable sort
  traverse enhanceApp apps

registerApp :: AppCreateDTO -> AppContextM AppDTO
registerApp reqDto = do
  runInTransaction $ do
    validateAppCreateDTO reqDto
    aUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    cloudDomain <- getCloudDomain
    let app = fromRegisterCreateDTO reqDto aUuid cloudDomain now
    insertApp app
    userUuid <- liftIO generateUuid
    let userCreate = U_Mapper.fromAppCreateToUserCreateDTO reqDto
    user <- createUserByAdminWithUuid userCreate userUuid (app ^. uuid) (app ^. clientUrl) True
    createAppConfig aUuid now
    createAppLimit aUuid now
    createSeederPersistentCommand aUuid (user ^. uuid) now
    return $ toDTO app Nothing Nothing

createAppByAdmin :: AppCreateDTO -> AppContextM AppDTO
createAppByAdmin reqDto = do
  runInTransaction $ do
    checkPermission _APP_PERM
    validateAppCreateDTO reqDto
    aUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    cloudDomain <- getCloudDomain
    let app = fromAdminCreateDTO reqDto aUuid cloudDomain now
    insertApp app
    userUuid <- liftIO generateUuid
    userPassword <- liftIO $ generateRandomString 25
    let userCreate = U_Mapper.fromAppCreateToUserCreateDTO reqDto & password .~ userPassword
    user <- createUserByAdminWithUuid userCreate userUuid (app ^. uuid) (app ^. clientUrl) False
    createAppConfig aUuid now
    createAppLimit aUuid now
    createSeederPersistentCommand aUuid (user ^. uuid) now
    return $ toDTO app Nothing Nothing

getAppById :: String -> AppContextM AppDetailDTO
getAppById aUuid = do
  checkPermission _APP_PERM
  app <- findAppById aUuid
  plans <- findAppPlansForAppUuid aUuid
  usage <- getUsage aUuid
  users <- findUsersWithAppFiltered aUuid [("role", _USER_ROLE_ADMIN)]
  return $ toDetailDTO app plans usage users

modifyApp :: String -> AppChangeDTO -> AppContextM App
modifyApp aUuid reqDto = do
  checkPermission _APP_PERM
  app <- findAppById aUuid
  validateAppChangeDTO app reqDto
  cloudDomain <- getCloudDomain
  let updatedApp = fromChangeDTO app reqDto cloudDomain
  updateAppById updatedApp

deleteApp :: String -> AppContextM ()
deleteApp aUuid = do
  checkPermission _APP_PERM
  _ <- findAppById aUuid
  deleteAppById aUuid
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
createAppConfig :: U.UUID -> UTCTime -> AppContextM AppConfig
createAppConfig aUuid now = do
  runInTransaction $ do
    let appConfig = (uuid .~ aUuid) . (createdAt .~ now) . (updatedAt .~ now) $ defaultAppConfig
    insertAppConfig appConfig
    return appConfig

createSeederPersistentCommand :: U.UUID -> U.UUID -> UTCTime -> AppContextM PersistentCommand
createSeederPersistentCommand aUuid createdBy now =
  runInTransaction $ do
    pUuid <- liftIO generateUuid
    let command =
          PCM.toPersistentCommand
            pUuid
            "data_seeder"
            "importDefaultData"
            (BSL.unpack . encode $ ImportDefaultDataCommand aUuid)
            1
            False
            aUuid
            createdBy
            now
    insertPersistentCommand command
    return command

getCloudDomain :: AppContextM String
getCloudDomain = do
  serverConfig <- asks _appContextServerConfig
  return $ fromMaybe "" (serverConfig ^. cloud . domain)
