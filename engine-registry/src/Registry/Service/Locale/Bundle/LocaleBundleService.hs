module Registry.Service.Locale.Bundle.LocaleBundleService where

import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Registry.Api.Resource.Locale.LocaleDTO
import Registry.Model.Context.AppContext
import Registry.S3.Locale.LocaleS3
import Registry.Service.Audit.AuditService
import Registry.Service.Locale.Bundle.LocaleBundleAcl
import Registry.Service.Locale.LocaleMapper
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Model.Locale.Locale
import Shared.Service.Locale.Bundle.LocaleBundleMapper

exportBundle :: String -> AppContextM BSL.ByteString
exportBundle lclId = do
  _ <- auditGetLocaleBundle lclId
  locale <- findLocaleById lclId
  content <- retrieveLocale locale.lId
  return $ toLocaleArchive locale content

importBundle :: BSL.ByteString -> AppContextM LocaleDTO
importBundle contentS = do
  checkWritePermission
  case fromLocaleArchive contentS of
    Right (bundle, content) -> do
      let locale = fromLocaleBundle bundle U.nil
      putLocale locale.lId content
      insertLocale locale
      return . toDTO [] $ locale
    Left error -> throwError error
