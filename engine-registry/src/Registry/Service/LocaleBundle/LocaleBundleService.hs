module Registry.Service.LocaleBundle.LocaleBundleService where

import qualified Data.ByteString.Lazy.Char8 as BSL

import Registry.Model.Context.AppContext
import Registry.S3.Locale.LocaleS3
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Model.Locale.Locale
import Shared.Service.LocaleBundle.LocaleBundleMapper

exportLocaleBundle :: String -> AppContextM BSL.ByteString
exportLocaleBundle lclId = do
  locale <- findLocaleById lclId
  content <- getLocale locale.lId
  return $ toLocaleArchive locale content
