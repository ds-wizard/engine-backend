module Wizard.Service.Tenant.Logo.LogoService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Hashable as H
import Data.Time

import Shared.Common.Util.Logger
import Shared.Common.Util.String (splitOn)
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.S3.Public.PublicS3
import Wizard.Service.Tenant.Config.ConfigMapper
import Wizard.Service.Tenant.Config.ConfigService

uploadLogo :: String -> String -> BSL.ByteString -> AppContextM ()
uploadLogo plainFileName contentType content =
  -- 1. Check permission
  runInTransaction $ do
    checkPermission _CFG_PERM
    -- 2. Get current config
    serverConfig <- asks serverConfig
    tenantConfig <- getCurrentTenantConfig
    -- 3. Upload logo
    let logoFileName = getLogoFileName plainFileName content
    if fmap extractFileName tenantConfig.lookAndFeel.logoUrl == Just logoFileName
      then logInfoI _CMP_SERVICE (f' "The logo is the same (%s). No action needed" [logoFileName])
      else do
        logInfoI _CMP_SERVICE (f' "Uploading logo with file ()..." [logoFileName])
        putPublicContent logoFileName (Just contentType) (BSL.toStrict content)
        logInfoI _CMP_SERVICE "Logo uploaded. Creating the public link..."
        newLogoUrl <- makePublicLink logoFileName
        logInfoI _CMP_SERVICE (f' "Public link for logo created (%s)" [newLogoUrl])
        -- 4. Prepare to update & validate
        now <- liftIO getCurrentTime
        let updatedTenantConfig = fromLogoDTO tenantConfig newLogoUrl now
        -- 5. Remove old logo if exists
        removeOldConfig "logo" tenantConfig tenantConfig.lookAndFeel.logoUrl
        -- 6. Update
        modifyTenantConfig updatedTenantConfig
        return ()

deleteLogo :: AppContextM ()
deleteLogo =
  -- 1. Check permission
  runInTransaction $ do
    checkPermission _CFG_PERM
    -- 2. Get current config
    serverConfig <- asks serverConfig
    tenantConfig <- getCurrentTenantConfig
    -- 3. Update database
    now <- liftIO getCurrentTime
    let updatedTenantConfig = fromLogoDeleteDTO tenantConfig now
    modifyTenantConfig updatedTenantConfig
    -- 6. Remove logo
    removeOldConfig "logo" tenantConfig tenantConfig.lookAndFeel.logoUrl

-- --------------------------------
-- PRIVATE
-- --------------------------------
getLogoFileName :: String -> BSL.ByteString -> String
getLogoFileName plainFileName content =
  let plainFileNameParts = splitOn "." plainFileName
      suffix =
        if length plainFileNameParts > 1
          then "." ++ plainFileNameParts !! (length plainFileNameParts - 1)
          else ""
      hash = show . abs . H.hash $ content
   in f' "logo.%s%s" [hash, suffix]
