module Registry.S3.Locale.LocaleS3 where

import qualified Data.ByteString.Char8 as BS
import Network.Minio

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.S3.Common
import Shared.Util.String (f')

folderName = "locales"

retrieveLocale :: String -> AppContextM BS.ByteString
retrieveLocale localeId = createGetObjectFn (f' "%s/%s" [folderName, localeId])

retrieveLocale' :: String -> AppContextM (Either MinioErr BS.ByteString)
retrieveLocale' localeId = createGetObjectFn' (f' "%s/%s" [folderName, localeId])

putLocale :: String -> BS.ByteString -> AppContextM String
putLocale localeId = createPutObjectFn (f' "%s/%s" [folderName, localeId]) Nothing Nothing

removeLocales :: AppContextM ()
removeLocales = createRemoveObjectFn folderName

removeLocale :: String -> AppContextM ()
removeLocale localeId = createRemoveObjectFn (f' "%s/%s" [folderName, localeId])
