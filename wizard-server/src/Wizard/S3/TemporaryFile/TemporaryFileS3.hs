module Wizard.S3.TemporaryFile.TemporaryFileS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit as C
import qualified Data.UUID as U
import Network.Minio

import Shared.Common.S3.Common
import Shared.Common.Util.String (f')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

folderName = "temporary-files"

retrieveTemporaryFile :: U.UUID -> String -> AppContextM BS.ByteString
retrieveTemporaryFile uuid fileName = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])

putTemporaryFile :: U.UUID -> String -> String -> String -> BS.ByteString -> AppContextM String
putTemporaryFile uuid fileName contentType contentDisposition = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName]) (Just contentType) (Just contentDisposition)

putTemporaryFileConduit :: U.UUID -> String -> String -> String -> Minio (C.ConduitM () BS.ByteString Minio ()) -> AppContextM String
putTemporaryFileConduit uuid fileName contentType contentDisposition = createPutObjectConduitFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName]) (Just contentType) (Just contentDisposition)

makeTemporaryFileLink :: U.UUID -> String -> AppContextM String
makeTemporaryFileLink uuid fileName = createMakePublicLink (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])

presigneGetTemporaryFileUrl :: U.UUID -> String -> Int -> AppContextM String
presigneGetTemporaryFileUrl uuid fileName = createPresignedGetObjectUrl (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])

removeTemporaryFiles :: AppContextM ()
removeTemporaryFiles = createRemoveObjectFn (f' "%s" [folderName])

removeTemporaryFile :: U.UUID -> String -> AppContextM ()
removeTemporaryFile uuid fileName = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])
