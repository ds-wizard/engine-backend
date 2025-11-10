module Registry.Service.Locale.Bundle.LocaleBundleService where

import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Registry.Model.Context.AppContext
import Registry.S3.Locale.LocaleS3
import Registry.Service.Audit.AuditService
import Registry.Service.Locale.Bundle.LocaleBundleAcl
import Registry.Service.Locale.LocaleMapper
import RegistryLib.Api.Resource.Locale.LocaleDTO
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Service.Locale.Bundle.LocaleBundleMapper

exportBundle :: String -> AppContextM BSL.ByteString
exportBundle lclId = do
  _ <- auditGetLocaleBundle lclId
  locale <- findLocaleById lclId
  wizardTranslation <- retrieveLocale locale.lId "wizard.json"
  mailTranslation <- retrieveLocale locale.lId "mail.po"
  return $ toLocaleArchive locale wizardTranslation mailTranslation

importBundle :: BSL.ByteString -> AppContextM LocaleDTO
importBundle contentS = do
  checkWritePermission
  case fromLocaleArchive contentS of
    Right (bundle, wizardTranslation, mailTranslation) -> do
      let locale = fromLocaleBundle bundle U.nil
      putLocale locale.lId "wizard.json" wizardTranslation
      putLocale locale.lId "mail.po" mailTranslation
      insertLocale locale
      return . toDTO [] $ locale
    Left error -> throwError error
