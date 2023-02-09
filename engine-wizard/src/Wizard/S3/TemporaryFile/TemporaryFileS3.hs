module Wizard.S3.TemporaryFile.TemporaryFileS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U

import Shared.S3.Common
import Shared.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "temporary-files"

retrieveTemporaryFile :: U.UUID -> String -> AppContextM BS.ByteString
retrieveTemporaryFile uuid fileName = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])

putTemporaryFile :: U.UUID -> String -> String -> BS.ByteString -> AppContextM String
putTemporaryFile uuid fileName contentType = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName]) (Just contentType)

makeTemporaryFileLink :: U.UUID -> String -> AppContextM String
makeTemporaryFileLink uuid = createMakePublicLink (f' "%s/%s" [folderName, U.toString uuid])

presigneGetTemporaryFileUrl :: U.UUID -> String -> Int -> AppContextM String
presigneGetTemporaryFileUrl uuid fileName = createPresignedGetObjectUrl (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])

removeTemporaryFiles :: AppContextM ()
removeTemporaryFiles = createRemoveObjectFn (f' "%s" [folderName])

removeTemporaryFile :: U.UUID -> String -> AppContextM ()
removeTemporaryFile uuid fileName = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])
