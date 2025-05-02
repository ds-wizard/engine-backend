module Wizard.S3.Locale.LocaleS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Common.S3.Common
import Shared.Common.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "locales"

retrieveLocale :: String -> String -> AppContextM BS.ByteString
retrieveLocale localeId filename = createGetObjectFn (f' "%s/%s/%s" [folderName, localeId, filename])

retrieveLocaleWithTenant :: U.UUID -> String -> String -> AppContextM BS.ByteString
retrieveLocaleWithTenant tenantUuid localeId fileName = createGetObjectWithTenantFn tenantUuid (f' "%s/%s/%s" [folderName, localeId, fileName])

putLocale :: String -> String -> BS.ByteString -> AppContextM String
putLocale localeId fileName = createPutObjectFn (f' "%s/%s/%s" [folderName, localeId, fileName]) Nothing Nothing

removeLocales :: AppContextM ()
removeLocales = createRemoveObjectFn folderName

removeLocale :: String -> AppContextM ()
removeLocale localeId = createRemoveObjectFn (f' "%s/%s" [folderName, localeId])
