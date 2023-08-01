module Wizard.Service.App.AppService where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Constant.User
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Crypto
import Shared.Common.Util.Uuid
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleDM
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import qualified Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper as PCM
import Wizard.Api.Resource.App.AppChangeDTO
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppDetailDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Database.DAO.Plan.AppPlanDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigDM
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.App.ImportDefaultDataCommand
import Wizard.Service.App.AppMapper
import Wizard.Service.App.AppUtil
import Wizard.Service.App.AppValidation
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Limit.AppLimitService
import Wizard.Service.Usage.UsageService
import qualified Wizard.Service.User.UserMapper as U_Mapper
import Wizard.Service.User.UserService
import WizardLib.Public.Model.PersistentCommand.App.CreateOrUpdateAppCommand

getAppsPage :: Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page AppDTO)
getAppsPage mQuery mEnabled pageable sort = do
  checkPermission _APP_PERM
  apps <- findAppsPage mQuery mEnabled pageable sort
  traverse enhanceApp apps

registerApp :: AppCreateDTO -> AppContextM AppDTO
registerApp reqDto = do
  runInTransaction $ do
    validateAppCreateDTO reqDto False
    aUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    serverConfig <- asks serverConfig
    let app = fromRegisterCreateDTO reqDto aUuid serverConfig now
    insertApp app
    userUuid <- liftIO generateUuid
    let userCreate = U_Mapper.fromAppCreateToUserCreateDTO reqDto
    user <- createUserByAdminWithUuid userCreate userUuid app.uuid app.clientUrl True
    createAppConfig aUuid now
    createAppLimit aUuid now
    createLocale aUuid now
    createSeederPersistentCommand aUuid user.uuid now
    return $ toDTO app Nothing Nothing

createAppByAdmin :: AppCreateDTO -> AppContextM AppDTO
createAppByAdmin reqDto = do
  runInTransaction $ do
    checkPermission _APP_PERM
    validateAppCreateDTO reqDto True
    aUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    serverConfig <- asks serverConfig
    let app = fromAdminCreateDTO reqDto aUuid serverConfig now
    insertApp app
    userUuid <- liftIO generateUuid
    userPassword <- liftIO $ generateRandomString 25
    let userCreate = U_Mapper.fromAppCreateToUserCreateDTO (reqDto {password = userPassword})
    user <- createUserByAdminWithUuid userCreate userUuid app.uuid app.clientUrl False
    createAppConfig aUuid now
    createAppLimit aUuid now
    createLocale aUuid now
    createSeederPersistentCommand aUuid user.uuid now
    return $ toDTO app Nothing Nothing

createAppByCommand :: CreateOrUpdateAppCommand -> AppContextM ()
createAppByCommand command = do
  now <- liftIO getCurrentTime
  serverConfig <- asks serverConfig
  let app = fromCommand command serverConfig now
  insertApp app
  createAppConfig app.uuid now
  createAppLimit app.uuid now
  createLocale app.uuid now
  createSeederPersistentCommand app.uuid systemUserUuid now
  return ()

getAppById :: U.UUID -> AppContextM AppDetailDTO
getAppById aUuid = do
  checkPermission _APP_PERM
  app <- findAppByUuid aUuid
  plans <- findAppPlansForAppUuid aUuid
  usage <- getUsage aUuid
  users <- findUsersWithAppFiltered aUuid [("role", _USER_ROLE_ADMIN)]
  appConfig <- getAppConfigByUuid app.uuid
  let mLogoUrl = appConfig.lookAndFeel.logoUrl
  let mPrimaryColor = appConfig.lookAndFeel.primaryColor
  return $ toDetailDTO app mLogoUrl mPrimaryColor plans usage users

modifyApp :: U.UUID -> AppChangeDTO -> AppContextM App
modifyApp aUuid reqDto = do
  checkPermission _APP_PERM
  app <- findAppByUuid aUuid
  validateAppChangeDTO app reqDto
  serverConfig <- asks serverConfig
  let updatedApp = fromChangeDTO app reqDto serverConfig
  updateAppByUuid updatedApp

deleteApp :: U.UUID -> AppContextM ()
deleteApp aUuid = do
  checkPermission _APP_PERM
  _ <- findAppByUuid aUuid
  deleteAppByUuid aUuid
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
createAppConfig :: U.UUID -> UTCTime -> AppContextM AppConfig
createAppConfig aUuid now = do
  runInTransaction $ do
    let appConfig =
          defaultAppConfig
            { uuid = aUuid
            , createdAt = now
            , updatedAt = now
            }
          :: AppConfig
    insertAppConfig appConfig
    return appConfig

createLocale :: U.UUID -> UTCTime -> AppContextM Locale
createLocale aUuid now = do
  runInTransaction $ do
    let locale =
          localeDefault
            { appUuid = aUuid
            , createdAt = now
            , updatedAt = now
            }
          :: Locale
    insertLocale locale
    return locale

createSeederPersistentCommand :: U.UUID -> U.UUID -> UTCTime -> AppContextM (PersistentCommand U.UUID)
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
            Nothing
            aUuid
            (Just createdBy)
            now
    insertPersistentCommand command
    return command
