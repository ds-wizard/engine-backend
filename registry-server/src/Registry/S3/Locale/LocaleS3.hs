module Registry.S3.Locale.LocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.Minio

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.S3.Common
import Shared.Common.Util.String (f')

folderName = "locales"

retrieveLocale :: U.UUID -> String -> AppContextM BS.ByteString
retrieveLocale localeUuid filename = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, filename])

retrieveLocale' :: U.UUID -> String -> AppContextM (Either MinioErr BS.ByteString)
retrieveLocale' localeUuid filename = createGetObjectFn' (f' "%s/%s/%s" [folderName, U.toString localeUuid, filename])

putLocale :: U.UUID -> String -> BS.ByteString -> AppContextM String
putLocale localeUuid fileName = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString localeUuid, fileName]) Nothing Nothing

removeLocales :: AppContextM ()
removeLocales = createRemoveObjectFn folderName

removeLocale :: U.UUID -> AppContextM ()
removeLocale localeUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString localeUuid])
