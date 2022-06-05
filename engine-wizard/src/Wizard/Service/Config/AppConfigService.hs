module Wizard.Service.Config.AppConfigService where

import Control.Lens ((&), (?~), (^.))
import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Hashable as H
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Util.String (splitOn)
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Integration.Http.Config.Runner
import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigEM ()
import Wizard.Model.Context.AppContext
import Wizard.S3.Public.PublicS3
import Wizard.Service.Acl.AclService
import Wizard.Service.App.AppHelper
import Wizard.Service.Config.AppConfigMapper
import Wizard.Service.Config.AppConfigValidation
import Wizard.Util.Logger

getAppConfig :: AppContextM AppConfig
getAppConfig = do
  serverConfig <- asks _appContextServerConfig
  encryptedAppConfig <- findAppConfig
  return $ process (serverConfig ^. general . secret) encryptedAppConfig

getAppConfigByUuid :: U.UUID -> AppContextM AppConfig
getAppConfigByUuid appUuid = do
  serverConfig <- asks _appContextServerConfig
  encryptedAppConfig <- findAppConfigByUuid appUuid
  return $ process (serverConfig ^. general . secret) encryptedAppConfig

getAppConfigDto :: AppContextM AppConfig
getAppConfigDto = do
  checkPermission _CFG_PERM
  getAppConfig

modifyAppConfig :: AppConfig -> AppContextM AppConfig
modifyAppConfig appConfig =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    let encryptedUpdatedAppConfig = process (serverConfig ^. general . secret) appConfig
    updateAppConfig encryptedUpdatedAppConfig
    return appConfig

modifyAppConfigDto :: AppConfigChangeDTO -> AppContextM AppConfig
modifyAppConfigDto reqDto
  -- 1. Check permission
 =
  runInTransaction $ do
    checkPermission _CFG_PERM
    -- 2. Get current config
    serverConfig <- asks _appContextServerConfig
    appConfig <- getAppConfig
    -- 3. Validate
    validateAppConfig reqDto
    -- 4. Prepare to update & validate
    now <- liftIO getCurrentTime
    let updatedAppConfig = fromChangeDTO reqDto appConfig now
    -- 5. Compile client CSS
    updatedAppConfigWithCss <-
      if colorsChanged appConfig updatedAppConfig && appConfig ^. feature . clientCustomizationEnabled
        then invokeClientCssCompilation appConfig updatedAppConfig
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
    serverConfig <- asks _appContextServerConfig
    appConfig <- getAppConfig
    -- 3. Prepare to update & validate
    now <- liftIO getCurrentTime
    let updatedAppConfig = fromClientCustomizationDTO appConfig newClientCustomizationEnabled now
    -- 5. Update
    modifyAppConfig updatedAppConfig
    -- 6. Create response
    return ()

invokeClientCssCompilation :: AppConfig -> AppConfig -> AppContextM AppConfig
invokeClientCssCompilation oldAppConfig newAppConfig
  -- 1. Recompile CSS
 = do
  logInfoU _CMP_SERVICE "Invoking compile of clients' CSS files..."
  app <- getCurrentApp
  cssContent <- compileClientCss app (newAppConfig ^. lookAndFeel)
  logInfoU _CMP_SERVICE "Compilation succeed"
  let cssFileName = f' "customization.%s.css" [show . abs . H.hash $ cssContent]
  logInfoU _CMP_SERVICE (f' "CSS filename: %s" [cssFileName])
  -- 2. Upload new CSS file
  logInfoU _CMP_SERVICE "Uploading new CSS file..."
  putPublicContent cssFileName (Just "text/css") (BSL.toStrict cssContent)
  logInfoU _CMP_SERVICE "CSS file uploaded. Creating the public link..."
  newStyleUrl <- makePublicLink cssFileName
  logInfoU _CMP_SERVICE (f' "Public link for CSS file created (%s)" [newStyleUrl])
  -- 3. Remove old CSS files if exists
  logInfoU _CMP_SERVICE "Compilation succeed"
  when ((oldAppConfig ^. lookAndFeel . styleUrl) /= Just newStyleUrl) (removeOldConfig "CSS file" oldAppConfig styleUrl)
  -- 4. Create response
  return $ newAppConfig & (lookAndFeel . styleUrl) ?~ newStyleUrl

-- --------------------------------
-- PRIVATE
-- --------------------------------
colorsChanged :: AppConfig -> AppConfig -> Bool
colorsChanged oldAppConfig newAppConfig =
  (oldAppConfig ^. lookAndFeel . primaryColor) /= (newAppConfig ^. lookAndFeel . primaryColor) ||
  (oldAppConfig ^. lookAndFeel . illustrationsColor) /=
  (newAppConfig ^. lookAndFeel . illustrationsColor)

removeOldConfig name appConfig urlPath =
  case appConfig ^. lookAndFeel . urlPath of
    Just url -> do
      logInfoU _CMP_SERVICE (f' "Deleting the old %s..." [name])
      let extractedFileName = extractFileName url
      logInfoU _CMP_SERVICE (f' "Extracted filename: %s" [extractedFileName])
      removePublic extractedFileName
      logInfoU _CMP_SERVICE (f' "The old %s deleted" [name])
    Nothing -> logInfoU _CMP_SERVICE (f' "There is no old %s" [name])

extractFileName :: String -> String
extractFileName url =
  let urlParts = splitOn "/" url
   in urlParts !! (length urlParts - 1)
