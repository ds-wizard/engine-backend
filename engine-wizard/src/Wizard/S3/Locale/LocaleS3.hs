module Wizard.S3.Locale.LocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.Minio

import Shared.S3.Common
import Shared.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "locales"

retrieveLocale :: String -> AppContextM BS.ByteString
retrieveLocale localeId = createGetObjectFn (f' "%s/%s" [folderName, localeId])

retrieveLocaleWithApp :: U.UUID -> String -> AppContextM BS.ByteString
retrieveLocaleWithApp appUuid localeId = createGetObjectWithAppFn appUuid (f' "%s/%s" [folderName, localeId])

retrieveLocale' :: String -> AppContextM (Either MinioErr BS.ByteString)
retrieveLocale' localeId = createGetObjectFn' (f' "%s/%s" [folderName, localeId])

putLocale :: String -> BS.ByteString -> AppContextM String
putLocale localeId = createPutObjectFn (f' "%s/%s" [folderName, localeId]) Nothing Nothing

removeLocales :: AppContextM ()
removeLocales = createRemoveObjectFn folderName

removeLocale :: String -> AppContextM ()
removeLocale localeId = createRemoveObjectFn (f' "%s/%s" [folderName, localeId])
