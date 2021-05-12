module Wizard.S3.Document.DocumentS3 where

import qualified Data.ByteString.Char8 as BS

import Shared.S3.Common
import Shared.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "documents"

getDocumentContent :: String -> AppContextM BS.ByteString
getDocumentContent documentUuid = createGetObjectFn (f' "%s/%s" [folderName, documentUuid])

putDocumentContent :: String -> BS.ByteString -> AppContextM ()
putDocumentContent documentUuid = createPutObjectFn (f' "%s/%s" [folderName, documentUuid])

removeDocumentContents :: AppContextM ()
removeDocumentContents = createRemoveObjectFn (f' "%s" [folderName])

removeDocumentContent :: String -> AppContextM ()
removeDocumentContent documentUuid = createRemoveObjectFn (f' "%s/%s" [folderName, documentUuid])
