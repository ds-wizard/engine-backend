module Wizard.Service.Config.AppConfigService where

import Control.Lens (Lens'(..), (.~), (^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Time

import LensesConfig
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigEM ()
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigMapper
import Wizard.Service.Server.ServerService

getAppConfigFeatures :: AppContextM AppConfigFeaturesDTO
getAppConfigFeatures = getAppConfigPart toFeaturesDTO features

modifyAppConfigFeatures :: AppConfigFeaturesDTO -> AppContextM AppConfigFeaturesDTO
modifyAppConfigFeatures = modifyAppPart fromFeaturesDTO toFeaturesDTO features

getAppConfigClient :: AppContextM AppConfigClientDTO
getAppConfigClient = getAppConfigPart toClientDTO client

modifyAppConfigClient :: AppConfigClientDTO -> AppContextM AppConfigClientDTO
modifyAppConfigClient = modifyAppPart fromClientDTO toClientDTO client

getAppConfigInfo :: AppContextM AppConfigInfoDTO
getAppConfigInfo = getAppConfigPart toInfoDTO info

modifyAppConfigInfo :: AppConfigInfoDTO -> AppContextM AppConfigInfoDTO
modifyAppConfigInfo = modifyAppPart fromInfoDTO toInfoDTO info

getAppConfigAffiliation :: AppContextM AppConfigAffiliationDTO
getAppConfigAffiliation = getAppConfigPart toAffiliationDTO affiliation

modifyAppConfigAffiliation :: AppConfigAffiliationDTO -> AppContextM AppConfigAffiliationDTO
modifyAppConfigAffiliation = modifyAppPart fromAffiliationDTO toAffiliationDTO affiliation

getAppConfigAuth :: AppContextM AppConfigAuthDTO
getAppConfigAuth = getAppConfigPart toAuthDTO auth

modifyAppConfigAuth :: AppConfigAuthDTO -> AppContextM AppConfigAuthDTO
modifyAppConfigAuth = modifyAppPart fromAuthDTO toAuthDTO auth

-- --------------------------------
-- PRIVATE
-- --------------------------------
getAppConfig :: AppContextM AppConfig
getAppConfig = do
  serverConfig <- asks _appContextApplicationConfig
  encryptedAppConfig <- findAppConfig
  return $ process (serverConfig ^. general . secret) encryptedAppConfig

modifyAppConfig :: AppConfig -> AppContextM ()
modifyAppConfig appConfig = do
  serverConfig <- asks _appContextApplicationConfig
  let encryptedAppConfig = process (serverConfig ^. general . secret) appConfig
  updateAppConfig encryptedAppConfig
  return ()

getAppConfigPart :: (a -> d) -> Lens' AppConfig a -> AppContextM d
getAppConfigPart mapperTo accessor = do
  appConfig <- getAppConfig
  return . mapperTo $ appConfig ^. accessor

modifyAppPart :: (d -> a) -> (a -> d) -> Lens' AppConfig a -> d -> AppContextM d
modifyAppPart mapperFrom mapperTo accessor reqDto = do
  appConfig <- getAppConfig
  now <- liftIO getCurrentTime
  let updatedAppPart = mapperFrom reqDto
  let updatedAppConfig = (accessor .~ updatedAppPart) . (updatedAt .~ now) $ appConfig
  modifyAppConfig updatedAppConfig
  restartServer
  return . mapperTo $ updatedAppConfig ^. accessor
