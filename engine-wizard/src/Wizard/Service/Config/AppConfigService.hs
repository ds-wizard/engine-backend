module Wizard.Service.Config.AppConfigService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Time

import LensesConfig
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigEM ()
import Wizard.Model.Context.AppContext
import Wizard.Service.Common.ACL
import Wizard.Service.Config.AppConfigMapper

getAppConfig :: AppContextM AppConfig
getAppConfig = do
  serverConfig <- asks _appContextServerConfig
  encryptedAppConfig <- findAppConfig
  return $ process (serverConfig ^. general . secret) encryptedAppConfig

getAppConfigDto :: AppContextM AppConfig
getAppConfigDto = do
  checkPermission _CFG_PERM
  getAppConfig

modifyAppConfig :: AppConfig -> AppContextM AppConfig
modifyAppConfig appConfig = do
  serverConfig <- asks _appContextServerConfig
  let encryptedUpdatedAppConfig = process (serverConfig ^. general . secret) appConfig
  updateAppConfig encryptedUpdatedAppConfig
  return appConfig

modifyAppConfigDto :: AppConfigChangeDTO -> AppContextM AppConfig
modifyAppConfigDto reqDto
  -- 1. Check permission
 = do
  checkPermission _CFG_PERM
  -- 2. Get current config
  serverConfig <- asks _appContextServerConfig
  appConfig <- getAppConfig
  -- 3. Prepare to update & validate
  now <- liftIO getCurrentTime
  let updatedAppConfig = fromChangeDTO reqDto appConfig now
  -- 4. Update
  modifyAppConfig updatedAppConfig
  -- 5. Create response
  return updatedAppConfig
