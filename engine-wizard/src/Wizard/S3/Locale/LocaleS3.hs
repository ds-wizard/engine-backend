module Wizard.S3.Locale.LocaleS3 where

import qualified Data.ByteString.Char8 as BS
import Network.Minio

import Shared.S3.Common
import Shared.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "locales"

getLocale :: String -> AppContextM BS.ByteString
getLocale localeId = createGetObjectFn (f' "%s/%s" [folderName, localeId])

getLocale' :: String -> AppContextM (Either MinioErr BS.ByteString)
getLocale' localeId = createGetObjectFn' (f' "%s/%s" [folderName, localeId])

putLocale :: String -> BS.ByteString -> AppContextM String
putLocale localeId = createPutObjectFn (f' "%s/%s" [folderName, localeId]) Nothing

removeLocales :: AppContextM ()
removeLocales = createRemoveObjectFn folderName

removeLocale :: String -> AppContextM ()
removeLocale localeId = createRemoveObjectFn (f' "%s/%s" [folderName, localeId])
