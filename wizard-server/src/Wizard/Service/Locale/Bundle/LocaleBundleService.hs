module Wizard.Service.Locale.Bundle.LocaleBundleService where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Error.Error
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleSimple
import Shared.Locale.Service.Locale.Bundle.LocaleBundleMapper
import Shared.Locale.Service.Locale.LocaleMapper
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Registry.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.S3.Locale.LocaleS3
import Wizard.Service.Locale.Bundle.LocaleBundleAudit
import Wizard.Service.Locale.LocaleValidation
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getTemporaryFileWithBundle :: U.UUID -> AppContextM TemporaryFileDTO
getTemporaryFileWithBundle uuid =
  runInTransaction $ do
    (coordinate, bundle) <- exportBundle uuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFile (f' "%s.zip" [show coordinate]) "application/octet-stream" mCurrentUserUuid bundle
    return $ TemporaryFileMapper.toDTO url "application/octet-stream"

exportBundle :: U.UUID -> AppContextM (Coordinate, BSL.ByteString)
exportBundle uuid =
  runInTransaction $ do
    checkPermission _LOC_PERM
    locale <- findLocaleByUuid uuid
    wizardTranslation <- retrieveLocale locale.uuid "wizard.json"
    mailTranslation <- retrieveLocale locale.uuid "mail.po"
    return (createCoordinate locale, toLocaleArchive locale wizardTranslation mailTranslation)

pullBundleFromRegistry :: Coordinate -> AppContextM LocaleSimple
pullBundleFromRegistry coordinate =
  runInTransaction $ do
    checkPermission _LOC_PERM
    lb <- catchError (retrieveLocaleBundleByCoordinate coordinate) handleError
    importBundle lb True
  where
    handleError error =
      if error == GeneralServerError (_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR "statusCode: 404")
        then throwError . UserError $ _ERROR_SERVICE_LB__PULL_NON_EXISTING_LOCALE (show coordinate)
        else throwError error

importBundle :: BSL.ByteString -> Bool -> AppContextM LocaleSimple
importBundle contentS fromRegistry =
  case fromLocaleArchive contentS of
    Right (bundle, wizardTranslation, mailTranslation) -> do
      validateLocaleIdUniqueness (createCoordinate bundle)
      uuid <- liftIO generateUuid
      tenantUuid <- asks currentTenantUuid
      let locale = fromLocaleBundle bundle uuid tenantUuid
      putLocale locale.uuid "wizard.json" wizardTranslation
      putLocale locale.uuid "mail.po" mailTranslation
      insertLocale locale
      if fromRegistry
        then auditLocaleBundlePullFromRegistry (createCoordinate locale)
        else auditLocaleBundleImportFromFile (createCoordinate locale)
      return . toSimple $ locale
    Left error -> throwError error
