module Registry.S3.Locale.LocaleS3 where

import qualified Data.ByteString.Char8 as BS
import Network.Minio

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.S3.Common
import Shared.Common.Util.String (f')

folderName = "locales"

retrieveLocale :: String -> String -> AppContextM BS.ByteString
retrieveLocale localeId filename = createGetObjectFn (f' "%s/%s/%s" [folderName, localeId, filename])

retrieveLocale' :: String -> String -> AppContextM (Either MinioErr BS.ByteString)
retrieveLocale' localeId filename = createGetObjectFn' (f' "%s/%s/%s" [folderName, localeId, filename])

putLocale :: String -> String -> BS.ByteString -> AppContextM String
putLocale localeId fileName = createPutObjectFn (f' "%s/%s/%s" [folderName, localeId, fileName]) Nothing Nothing

removeLocales :: AppContextM ()
removeLocales = createRemoveObjectFn folderName

removeLocale :: String -> AppContextM ()
removeLocale localeId = createRemoveObjectFn (f' "%s/%s" [folderName, localeId])
