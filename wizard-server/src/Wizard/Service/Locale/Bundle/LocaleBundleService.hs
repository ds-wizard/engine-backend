module Wizard.Service.Locale.Bundle.LocaleBundleService where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Error.Error
import Shared.Common.Util.String
import Shared.Locale.Api.Resource.LocaleBundle.LocaleBundleDTO
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.S3.Locale.LocaleS3
import Wizard.Service.Locale.Bundle.LocaleBundleAudit
import Wizard.Service.Locale.LocaleMapper
import Wizard.Service.Locale.LocaleValidation
import Wizard.Service.Tenant.Config.ConfigService
import WizardLib.Locale.Service.Locale.Bundle.LocaleBundleMapper
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getTemporaryFileWithBundle :: String -> AppContextM TemporaryFileDTO
getTemporaryFileWithBundle lclId =
  runInTransaction $ do
    bundle <- exportBundle lclId
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile (f' "%s.zip" [lclId]) "application/octet-stream" mCurrentUserUuid bundle
    return $ TemporaryFileMapper.toDTO url "application/octet-stream"

exportBundle :: String -> AppContextM BSL.ByteString
exportBundle lclId =
  runInTransaction $ do
    checkPermission _LOC_PERM
    locale <- findLocaleById lclId
    content <- retrieveLocale locale.lId
    return $ toLocaleArchive locale content

pullBundleFromRegistry :: String -> AppContextM ()
pullBundleFromRegistry lclId =
  runInTransaction $ do
    checkPermission _LOC_PERM
    lb <- catchError (retrieveLocaleBundleById lclId) handleError
    _ <- importBundle lb True
    return ()
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_LB__PULL_NON_EXISTING_LOCALE lclId
        else throwError error

importBundle :: BSL.ByteString -> Bool -> AppContextM LocaleDTO
importBundle contentS fromRegistry =
  case fromLocaleArchive contentS of
    Right (bundle, content) -> do
      validateLocaleIdUniqueness bundle.lId
      tenantUuid <- asks currentTenantUuid
      let locale = fromLocaleBundle bundle tenantUuid
      putLocale locale.lId content
      insertLocale locale
      if fromRegistry
        then auditLocaleBundlePullFromRegistry locale.lId
        else auditLocaleBundleImportFromFile locale.lId
      tenantConfig <- getCurrentTenantConfig
      return . toDTO tenantConfig.registry.enabled $ toLocaleList locale
    Left error -> throwError error
