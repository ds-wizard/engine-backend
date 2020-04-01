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
import Wizard.Service.Config.AppConfigMapper

getAppConfig :: AppContextM AppConfig
getAppConfig = do
  serverConfig <- asks _appContextServerConfig
  encryptedAppConfig <- findAppConfig
  return $ process (serverConfig ^. general . secret) encryptedAppConfig

modifyAppConfig :: AppConfigChangeDTO -> AppContextM AppConfig
modifyAppConfig reqDto
  -- 1. Get current config
 = do
  serverConfig <- asks _appContextServerConfig
  appConfig <- getAppConfig
  -- 2. Prepare to update & validate
  now <- liftIO getCurrentTime
  let updatedAppConfig = fromChangeDTO reqDto appConfig now
  -- 3. Update
  let encryptedUpdatedAppConfig = process (serverConfig ^. general . secret) updatedAppConfig
  updateAppConfig encryptedUpdatedAppConfig
  -- 4. Create response
  return updatedAppConfig
