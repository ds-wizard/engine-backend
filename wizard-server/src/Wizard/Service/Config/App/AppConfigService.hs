module Wizard.Service.Config.App.AppConfigService where

import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Hashable as H
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Util.Logger
import Shared.Common.Util.String (splitOn)
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Integration.Http.Config.Runner
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigEM ()
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.S3.Public.PublicS3
import Wizard.Service.Acl.AclService
import Wizard.Service.App.AppHelper
import Wizard.Service.Config.App.AppConfigAudit
import Wizard.Service.Config.App.AppConfigMapper
import Wizard.Service.Config.App.AppConfigValidation

getAppConfig :: AppContextM AppConfig
getAppConfig = do
  serverConfig <- asks serverConfig
  encryptedAppConfig <- findAppConfig
  return $ process serverConfig.general.secret encryptedAppConfig

getAppConfigByUuid :: U.UUID -> AppContextM AppConfig
getAppConfigByUuid appUuid = do
  serverConfig <- asks serverConfig
  encryptedAppConfig <- findAppConfigByUuid appUuid
  return $ process serverConfig.general.secret encryptedAppConfig

getAppConfigDto :: AppContextM AppConfig
getAppConfigDto = do
  checkPermission _CFG_PERM
  getAppConfig

modifyAppConfig :: AppConfig -> AppContextM AppConfig
modifyAppConfig appConfig =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let encryptedUpdatedAppConfig = process serverConfig.general.secret appConfig
    updateAppConfig encryptedUpdatedAppConfig
    return appConfig

modifyAppConfigDto :: AppConfigChangeDTO -> AppContextM AppConfig
modifyAppConfigDto reqDto =
  -- 1. Check permission
  runInTransaction $ do
    checkPermission _CFG_PERM
    -- 2. Get current config
    serverConfig <- asks serverConfig
    appConfig <- getAppConfig
    -- 3. Validate
    validateAppConfig reqDto
    -- 4. Prepare to update & validate
    now <- liftIO getCurrentTime
    let updatedAppConfig = fromChangeDTO reqDto appConfig now
    -- 5. Compile client CSS
    updatedAppConfigWithCss <-
      if colorsChanged appConfig updatedAppConfig && appConfig.feature.clientCustomizationEnabled
        then do
          auditAppConfigChangeColors $ appConfig.uuid
          invokeClientCssCompilation appConfig updatedAppConfig
        else return updatedAppConfig
    -- 6. Update
    modifyAppConfig updatedAppConfigWithCss
    -- 7. Create response
    return updatedAppConfigWithCss

modifyClientCustomization :: Bool -> AppContextM ()
modifyClientCustomization newClientCustomizationEnabled = do
  runInTransaction $
    -- 1. Check permission
    do
      checkPermission _CFG_PERM
      -- 2. Get current config
      serverConfig <- asks serverConfig
      appConfig <- getAppConfig
      -- 3. Prepare to update & validate
      now <- liftIO getCurrentTime
      let updatedAppConfig = fromClientCustomizationDTO appConfig newClientCustomizationEnabled now
      -- 5. Update
      modifyAppConfig updatedAppConfig
      -- 6. Create response
      return ()

invokeClientCssCompilation :: AppConfig -> AppConfig -> AppContextM AppConfig
invokeClientCssCompilation oldAppConfig newAppConfig =
  -- 1. Recompile CSS
  do
    logInfoI _CMP_SERVICE "Invoking compile of clients' CSS files..."
    app <- getCurrentApp
    cssContent <- compileClientCss app newAppConfig.lookAndFeel
    logInfoI _CMP_SERVICE "Compilation succeed"
    let cssFileName = f' "customization.%s.css" [show . abs . H.hash $ cssContent]
    logInfoI _CMP_SERVICE (f' "CSS filename: %s" [cssFileName])
    -- 2. Upload new CSS file
    logInfoI _CMP_SERVICE "Uploading new CSS file..."
    putPublicContent cssFileName (Just "text/css") (BSL.toStrict cssContent)
    logInfoI _CMP_SERVICE "CSS file uploaded. Creating the public link..."
    newStyleUrl <- makePublicLink cssFileName
    logInfoI _CMP_SERVICE (f' "Public link for CSS file created (%s)" [newStyleUrl])
    -- 3. Remove old CSS files if exists
    logInfoI _CMP_SERVICE "Compilation succeed"
    when (oldAppConfig.lookAndFeel.styleUrl /= Just newStyleUrl) (removeOldConfig "CSS file" oldAppConfig oldAppConfig.lookAndFeel.styleUrl)
    -- 4. Create response
    return $ newAppConfig {lookAndFeel = newAppConfig.lookAndFeel {styleUrl = Just newStyleUrl}}

-- --------------------------------
-- PRIVATE
-- --------------------------------
colorsChanged :: AppConfig -> AppConfig -> Bool
colorsChanged oldAppConfig newAppConfig =
  oldAppConfig.lookAndFeel.primaryColor /= newAppConfig.lookAndFeel.primaryColor
    || oldAppConfig.lookAndFeel.illustrationsColor
      /= newAppConfig.lookAndFeel.illustrationsColor

removeOldConfig name appConfig urlPath =
  case urlPath of
    Just url -> do
      logInfoI _CMP_SERVICE (f' "Deleting the old %s..." [name])
      let extractedFileName = extractFileName url
      logInfoI _CMP_SERVICE (f' "Extracted filename: %s" [extractedFileName])
      removePublic extractedFileName
      logInfoI _CMP_SERVICE (f' "The old %s deleted" [name])
    Nothing -> logInfoI _CMP_SERVICE (f' "There is no old %s" [name])

extractFileName :: String -> String
extractFileName url =
  let urlParts = splitOn "/" url
   in urlParts !! (length urlParts - 1)
