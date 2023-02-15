module Registry.Service.LocaleBundle.LocaleBundleService where

import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import Registry.Api.Resource.Locale.LocaleDTO
import Registry.Model.Context.AppContext
import Registry.S3.Locale.LocaleS3
import Registry.Service.Locale.LocaleMapper
import Registry.Service.LocaleBundle.LocaleBundleAcl
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Model.Locale.Locale
import Shared.Service.LocaleBundle.LocaleBundleMapper

exportLocaleBundle :: String -> AppContextM BSL.ByteString
exportLocaleBundle lclId = do
  locale <- findLocaleById lclId
  content <- retrieveLocale locale.lId
  return $ toLocaleArchive locale content

importLocaleBundle :: BSL.ByteString -> AppContextM LocaleDTO
importLocaleBundle contentS = do
  checkWritePermission
  case fromLocaleArchive contentS of
    Right (bundle, content) -> do
      let locale = fromLocaleBundle bundle U.nil
      putLocale locale.lId content
      insertLocale locale
      return . toDTO [] $ locale
    Left error -> throwError error
