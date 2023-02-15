module Wizard.Service.LocaleBundle.LocaleBundleService where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Shared.Api.Resource.LocaleBundle.LocaleBundleDTO
import Shared.Model.Error.Error
import Shared.Model.Locale.Locale
import Shared.Service.LocaleBundle.LocaleBundleMapper
import Shared.Util.String
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.TemporaryFile.TemporaryFileDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Locale.LocaleDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Locale.LocaleState
import Wizard.S3.Locale.LocaleS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Locale.LocaleMapper
import Wizard.Service.Locale.LocaleValidation
import Wizard.Service.LocaleBundle.LocaleBundleAudit
import qualified Wizard.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import Wizard.Service.TemporaryFile.TemporaryFileService

getTemporaryFileWithLocaleBundle :: String -> AppContextM TemporaryFileDTO
getTemporaryFileWithLocaleBundle lclId =
  runInTransaction $ do
    bundle <- exportLocaleBundle lclId
    url <- createTemporaryFile (f' "%s.zip" [lclId]) "application/octet-stream" bundle
    return $ TemporaryFileMapper.toDTO url "application/octet-stream"

exportLocaleBundle :: String -> AppContextM BSL.ByteString
exportLocaleBundle lclId =
  runInTransaction $ do
    locale <- findLocaleById lclId
    content <- retrieveLocale locale.lId
    return $ toLocaleArchive locale content

pullLocaleBundleFromRegistry :: String -> AppContextM ()
pullLocaleBundleFromRegistry lclId =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    lb <- catchError (retrieveLocaleBundleById lclId) handleError
    _ <- importAndConvertLocaleBundle lb True
    return ()
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_LB__PULL_NON_EXISTING_LOCALE lclId
        else throwError error

importAndConvertLocaleBundle :: BSL.ByteString -> Bool -> AppContextM LocaleDTO
importAndConvertLocaleBundle contentS fromRegistry =
  case fromLocaleArchive contentS of
    Right (bundle, content) -> do
      validateLocaleIdUniqueness bundle.lId
      appUuid <- asks currentAppUuid
      let locale = fromLocaleBundle bundle appUuid
      putLocale locale.lId content
      insertLocale locale
      if fromRegistry
        then auditLocaleBundlePullFromRegistry locale.lId
        else auditLocaleBundleImportFromFile locale.lId
      appConfig <- getAppConfig
      return . toDTO appConfig.registry.enabled $ toLocaleList locale UnknownLocaleState
    Left error -> throwError error
