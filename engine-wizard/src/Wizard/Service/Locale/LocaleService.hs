module Wizard.Service.Locale.LocaleService where

import Control.Monad.Except (throwError)
import qualified Data.ByteString.Char8 as BS
import Network.Minio

import Shared.Localization.Messages.Internal
import Shared.Model.Error.Error
import Shared.Util.String
import Wizard.Model.Context.AppContext
import Wizard.S3.Locale.LocaleS3

getLocaleForId :: String -> AppContextM BS.ByteString
getLocaleForId localeId = tryLocale localeId tryJustCountry
  where
    tryJustCountry = do
      let localeIdParts = splitOn "-" localeId
      if length localeIdParts > 1
        then tryLocale (head localeIdParts) tryDefault
        else tryDefault
    tryDefault = tryLocale "default" (return . BS.pack $ "{}")

-- --------------------------------
-- PRIVATE
-- --------------------------------
tryLocale :: String -> AppContextM BS.ByteString -> AppContextM BS.ByteString
tryLocale localeId errorCallback = do
  eLocale <- getLocale' (f' "%s.json" [localeId])
  case eLocale of
    Right locale -> return locale
    Left (MErrService NoSuchKey) -> errorCallback
    Left error -> throwError . GeneralServerError $ _ERROR_S3__GENERIC_ERROR (show error)
