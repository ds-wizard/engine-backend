module Wizard.Service.Config.AppConfigLogoService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Hashable as H
import Data.Time

import Shared.Util.String (splitOn)
import Wizard.Database.DAO.Common
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.S3.Public.PublicS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Common
import Wizard.Service.Config.AppConfigMapper
import Wizard.Service.Config.AppConfigService
import Wizard.Util.Logger

uploadLogo :: String -> String -> BSL.ByteString -> AppContextM ()
uploadLogo plainFileName contentType content =
  -- 1. Check permission
  runInTransaction $ do
    checkPermission _CFG_PERM
    checkIfClientCustomizationIsEnabled
    -- 2. Get current config
    serverConfig <- asks serverConfig
    appConfig <- getAppConfig
    -- 3. Upload logo
    let logoFileName = getLogoFileName plainFileName content
    if fmap extractFileName appConfig.lookAndFeel.logoUrl == Just logoFileName
      then logInfoU _CMP_SERVICE (f' "The logo is the same (%s). No action needed" [logoFileName])
      else do
        logInfoU _CMP_SERVICE (f' "Uploading logo with file ()..." [logoFileName])
        putPublicContent logoFileName (Just contentType) (BSL.toStrict content)
        logInfoU _CMP_SERVICE "Logo uploaded. Creating the public link..."
        newLogoUrl <- makePublicLink logoFileName
        logInfoU _CMP_SERVICE (f' "Public link for logo created (%s)" [newLogoUrl])
        -- 4. Prepare to update & validate
        now <- liftIO getCurrentTime
        let updatedAppConfig = fromLogoDTO appConfig newLogoUrl now
        -- 5. Compile Client CSS
        updatedAppConfigWithCss <- invokeClientCssCompilation appConfig updatedAppConfig
        -- 6. Remove old logo if exists
        removeOldConfig "logo" appConfig appConfig.lookAndFeel.logoUrl
        -- 7. Update
        modifyAppConfig updatedAppConfigWithCss
        return ()

deleteLogo :: AppContextM ()
deleteLogo =
  -- 1. Check permission
  runInTransaction $ do
    checkPermission _CFG_PERM
    checkIfClientCustomizationIsEnabled
    -- 2. Get current config
    serverConfig <- asks serverConfig
    appConfig <- getAppConfig
    -- 3. Update database
    now <- liftIO getCurrentTime
    let updatedAppConfig = fromLogoDeleteDTO appConfig now
    updatedAppConfigWithCss <- invokeClientCssCompilation appConfig updatedAppConfig
    modifyAppConfig updatedAppConfigWithCss
    -- 6. Remove logo
    removeOldConfig "logo" appConfig appConfig.lookAndFeel.logoUrl

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

checkIfClientCustomizationIsEnabled =
  checkIfAppFeatureIsEnabled "Client Customization" (\c -> c.feature.clientCustomizationEnabled)
