module WizardLib.Public.S3.TemporaryFile.TemporaryFileS3 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit as C
import qualified Data.UUID as U
import Network.Minio

import Shared.Common.Model.Context.AppContext
import Shared.Common.S3.Common
import Shared.Common.Util.String (f')

folderName = "temporary-files"

retrieveTemporaryFile :: AppContextC s sc m => U.UUID -> String -> m BS.ByteString
retrieveTemporaryFile uuid fileName = createGetObjectFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])

putTemporaryFile :: AppContextC s sc m => U.UUID -> String -> String -> String -> BS.ByteString -> m String
putTemporaryFile uuid fileName contentType contentDisposition = createPutObjectFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName]) (Just contentType) (Just contentDisposition)

putTemporaryFileConduit :: AppContextC s sc m => U.UUID -> String -> String -> String -> Minio (C.ConduitM () BS.ByteString Minio ()) -> m String
putTemporaryFileConduit uuid fileName contentType contentDisposition = createPutObjectConduitFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName]) (Just contentType) (Just contentDisposition)

makeTemporaryFileLink :: AppContextC s sc m => U.UUID -> String -> m String
makeTemporaryFileLink uuid fileName = createMakePublicLink (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])

presigneGetTemporaryFileUrl :: AppContextC s sc m => U.UUID -> String -> Int -> m String
presigneGetTemporaryFileUrl uuid fileName = createPresignedGetObjectUrl (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])

removeTemporaryFiles :: AppContextC s sc m => m ()
removeTemporaryFiles = createRemoveObjectFn (f' "%s" [folderName])

removeTemporaryFile :: AppContextC s sc m => U.UUID -> String -> m ()
removeTemporaryFile uuid fileName = createRemoveObjectFn (f' "%s/%s/%s" [folderName, U.toString uuid, fileName])
