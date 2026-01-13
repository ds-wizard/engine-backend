module Registry.Service.Locale.Bundle.LocaleBundleService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Registry.Model.Context.AppContext
import Registry.S3.Locale.LocaleS3
import Registry.Service.Audit.AuditService
import Registry.Service.Locale.Bundle.LocaleBundleAcl
import Registry.Service.Locale.LocaleMapper
import RegistryLib.Api.Resource.Locale.LocaleDTO
import Shared.Common.Util.Uuid
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Service.Locale.Bundle.LocaleBundleMapper

exportBundle :: Coordinate -> AppContextM BSL.ByteString
exportBundle lId = do
  _ <- auditGetLocaleBundle lId
  locale <- findLocaleByCoordinate lId
  wizardTranslation <- retrieveLocale locale.uuid "wizard.json"
  mailTranslation <- retrieveLocale locale.uuid "mail.po"
  return $ toLocaleArchive locale wizardTranslation mailTranslation

importBundle :: BSL.ByteString -> AppContextM LocaleDTO
importBundle contentS = do
  checkWritePermission
  case fromLocaleArchive contentS of
    Right (bundle, wizardTranslation, mailTranslation) -> do
      uuid <- liftIO generateUuid
      let locale = fromLocaleBundle bundle uuid U.nil
      putLocale locale.uuid "wizard.json" wizardTranslation
      putLocale locale.uuid "mail.po" mailTranslation
      insertLocale locale
      return . toDTO [] $ locale
    Left error -> throwError error
