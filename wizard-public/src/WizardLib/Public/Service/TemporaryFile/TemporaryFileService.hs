module WizardLib.Public.Service.TemporaryFile.TemporaryFileService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Conduit as C
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import Network.Minio
import Network.URI.Encode (encode)

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import WizardLib.Public.Database.DAO.TemporaryFile.TemporaryFileDAO
import WizardLib.Public.Model.TemporaryFile.TemporaryFile
import WizardLib.Public.S3.TemporaryFile.TemporaryFileS3
import WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper

createTemporaryFile :: AppContextC s sc m => String -> String -> Maybe U.UUID -> BSL.ByteString -> m String
createTemporaryFile fileName contentType mCreatedBy content = do
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let expirationInSeconds = 60
    let escapedFileName = filter isLetterOrDotOrDashOrUnderscore fileName
    let tf = toTemporaryFile uuid escapedFileName contentType expirationInSeconds tenantUuid mCreatedBy now
    insertTemporaryFile tf
    let contentDisposition = f' "attachment;filename=\"%s\"" [trim fileName]
    putTemporaryFile tf.uuid escapedFileName tf.contentType contentDisposition (BSL.toStrict content)
    presignGetTemporaryFileUrl tf.uuid escapedFileName expirationInSeconds

createTemporaryFileConduit :: AppContextC s sc m => String -> String -> Maybe U.UUID -> Minio (C.ConduitM () BS.ByteString Minio ()) -> m String
createTemporaryFileConduit fileName contentType mCreatedBy contentAction = do
  runInTransaction logInfoI logWarnI $ do
    uuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let expirationInSeconds = 60
    let escapedFileName = filter isLetterOrDotOrDashOrUnderscore fileName
    let tf = toTemporaryFile uuid escapedFileName contentType expirationInSeconds tenantUuid mCreatedBy now
    insertTemporaryFile tf
    let contentDisposition = f' "attachment;filename=\"%s\"" [trim fileName]
    putTemporaryFileConduit tf.uuid escapedFileName tf.contentType contentDisposition contentAction
    presignGetTemporaryFileUrl tf.uuid escapedFileName expirationInSeconds

deleteTemporaryFile :: AppContextC s sc m => TemporaryFile -> m ()
deleteTemporaryFile tf = do
  deleteTemporaryFileByUuid tf.uuid
  let escapedFileName = encode tf.fileName
  removeTemporaryFile tf.uuid escapedFileName

cleanTemporaryFiles :: AppContextC s sc m => m ()
cleanTemporaryFiles =
  runInTransaction logInfoI logWarnI $ do
    now <- liftIO getCurrentTime
    tfs <- findTemporaryFilesOlderThen now
    traverse_ deleteTemporaryFile tfs
