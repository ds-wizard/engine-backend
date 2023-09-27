module Wizard.Service.Tenant.Config.ConfigService where

import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Hashable as H
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Util.Logger
import Shared.Common.Util.String (splitOn)
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.TenantConfigDAO
import Wizard.Integration.Http.Config.Runner
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigEM ()
import Wizard.S3.Public.PublicS3
import Wizard.Service.Tenant.Config.ConfigAudit
import Wizard.Service.Tenant.Config.ConfigMapper
import Wizard.Service.Tenant.Config.ConfigValidation

getCurrentTenantConfig :: AppContextM TenantConfig
getCurrentTenantConfig = do
  serverConfig <- asks serverConfig
  encryptedTenantConfig <- findCurrentTenantConfig
  return $ process serverConfig.general.secret encryptedTenantConfig

getTenantConfigByUuid :: U.UUID -> AppContextM TenantConfig
getTenantConfigByUuid tenantUuid = do
  serverConfig <- asks serverConfig
  encryptedTenantConfig <- findCurrentTenantConfigByUuid tenantUuid
  return $ process serverConfig.general.secret encryptedTenantConfig

getCurrentTenantConfigDto :: AppContextM TenantConfig
getCurrentTenantConfigDto = do
  checkPermission _CFG_PERM
  getCurrentTenantConfig

modifyTenantConfig :: TenantConfig -> AppContextM TenantConfig
modifyTenantConfig tenantConfig =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let encryptedUpdatedTenantConfig = process serverConfig.general.secret tenantConfig
    updateTenantConfig encryptedUpdatedTenantConfig
    return tenantConfig

modifyTenantConfigDto :: TenantConfigChangeDTO -> AppContextM TenantConfig
modifyTenantConfigDto reqDto =
  -- 1. Check permission
  runInTransaction $ do
    checkPermission _CFG_PERM
    -- 2. Get current config
    serverConfig <- asks serverConfig
    tenantConfig <- getCurrentTenantConfig
    -- 3. Validate
    validateTenantConfig reqDto
    -- 4. Prepare to update & validate
    now <- liftIO getCurrentTime
    let updatedTenantConfig = fromChangeDTO reqDto tenantConfig now
    -- 5. Compile client CSS
    updatedTenantConfigWithCss <-
      if colorsChanged tenantConfig updatedTenantConfig && tenantConfig.feature.clientCustomizationEnabled
        then do
          auditChangeColors $ tenantConfig.uuid
          invokeClientCssCompilation tenantConfig updatedTenantConfig
        else return updatedTenantConfig
    -- 6. Update
    modifyTenantConfig updatedTenantConfigWithCss
    -- 7. Create response
    return updatedTenantConfigWithCss

modifyClientCustomization :: Bool -> AppContextM ()
modifyClientCustomization newClientCustomizationEnabled = do
  runInTransaction $
    -- 1. Check permission
    do
      checkPermission _CFG_PERM
      -- 2. Get current config
      serverConfig <- asks serverConfig
      tenantConfig <- getCurrentTenantConfig
      -- 3. Prepare to update & validate
      now <- liftIO getCurrentTime
      let updatedTenantConfig = fromClientCustomizationDTO tenantConfig newClientCustomizationEnabled now
      -- 5. Update
      modifyTenantConfig updatedTenantConfig
      -- 6. Create response
      return ()

invokeClientCssCompilation :: TenantConfig -> TenantConfig -> AppContextM TenantConfig
invokeClientCssCompilation oldTenantConfig newTenantConfig =
  -- 1. Recompile CSS
  do
    logInfoI _CMP_SERVICE "Invoking compile of clients' CSS files..."
    cssContent <- compileClientCss newTenantConfig.lookAndFeel
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
    when (oldTenantConfig.lookAndFeel.styleUrl /= Just newStyleUrl) (removeOldConfig "CSS file" oldTenantConfig oldTenantConfig.lookAndFeel.styleUrl)
    -- 4. Create response
    return $ newTenantConfig {lookAndFeel = newTenantConfig.lookAndFeel {styleUrl = Just newStyleUrl}}

-- --------------------------------
-- PRIVATE
-- --------------------------------
colorsChanged :: TenantConfig -> TenantConfig -> Bool
colorsChanged oldTenantConfig newTenantConfig =
  oldTenantConfig.lookAndFeel.primaryColor /= newTenantConfig.lookAndFeel.primaryColor
    || oldTenantConfig.lookAndFeel.illustrationsColor
      /= newTenantConfig.lookAndFeel.illustrationsColor

removeOldConfig name tenantConfig urlPath =
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
