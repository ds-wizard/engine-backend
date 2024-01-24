module Wizard.S3.Document.DocumentS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.Common.S3.Common
import Shared.Common.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "documents"

retrieveDocumentContent :: U.UUID -> AppContextM BS.ByteString
retrieveDocumentContent documentUuid = createGetObjectFn (f' "%s/%s" [folderName, U.toString documentUuid])

putDocumentContent :: U.UUID -> BS.ByteString -> AppContextM String
putDocumentContent documentUuid = createPutObjectFn (f' "%s/%s" [folderName, U.toString documentUuid]) Nothing Nothing

presigneGetDocumentUrl :: U.UUID -> Int -> AppContextM String
presigneGetDocumentUrl documentUuid = createPresignedGetObjectUrl (f' "%s/%s" [folderName, U.toString documentUuid])

removeDocumentContents :: AppContextM ()
removeDocumentContents = createRemoveObjectFn folderName

removeDocumentContent :: U.UUID -> AppContextM ()
removeDocumentContent documentUuid = createRemoveObjectFn (f' "%s/%s" [folderName, U.toString documentUuid])

removeDocumentContentWithTenant :: U.UUID -> U.UUID -> AppContextM ()
removeDocumentContentWithTenant tenantUuid documentUuid = createRemoveObjectWithTenantFn tenantUuid (f' "%s/%s" [folderName, U.toString documentUuid])
