module Wizard.Service.TemporaryFile.TemporaryFileService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Conduit as C
import Data.Foldable (traverse_)
import Data.Time
import Network.Minio
import Network.URI.Encode (encode)

import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.TemporaryFile.TemporaryFileDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.TemporaryFile.TemporaryFile
import Wizard.S3.TemporaryFile.TemporaryFileS3
import Wizard.Service.TemporaryFile.TemporaryFileMapper

createTemporaryFile :: String -> String -> BSL.ByteString -> AppContextM String
createTemporaryFile fileName contentType content = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    mCurrentUser <- asks currentUser
    now <- liftIO getCurrentTime
    let expirationInSeconds = 60
    let escapedFileName = filter isLetterOrDotOrDashOrUnderscore fileName
    let tf = toTemporaryFile uuid escapedFileName contentType expirationInSeconds tenantUuid mCurrentUser now
    insertTemporaryFile tf
    let contentDisposition = f' "attachment;filename=\"%s\"" [trim fileName]
    putTemporaryFile tf.uuid escapedFileName tf.contentType contentDisposition (BSL.toStrict content)
    presigneGetTemporaryFileUrl tf.uuid escapedFileName expirationInSeconds

createTemporaryFileConduit :: String -> String -> Minio (C.ConduitM () BS.ByteString Minio ()) -> AppContextM String
createTemporaryFileConduit fileName contentType contentAction = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    mCurrentUser <- asks currentUser
    now <- liftIO getCurrentTime
    let expirationInSeconds = 60
    let escapedFileName = filter isLetterOrDotOrDashOrUnderscore fileName
    let tf = toTemporaryFile uuid escapedFileName contentType expirationInSeconds tenantUuid mCurrentUser now
    insertTemporaryFile tf
    let contentDisposition = f' "attachment;filename=\"%s\"" [trim fileName]
    putTemporaryFileConduit tf.uuid escapedFileName tf.contentType contentDisposition contentAction
    presigneGetTemporaryFileUrl tf.uuid escapedFileName expirationInSeconds

deleteTemporaryFile :: TemporaryFile -> AppContextM ()
deleteTemporaryFile tf = do
  deleteTemporaryFileByUuid tf.uuid
  let escapedFileName = encode tf.fileName
  removeTemporaryFile tf.uuid escapedFileName

cleanTemporaryFiles :: AppContextM ()
cleanTemporaryFiles =
  runInTransaction $ do
    now <- liftIO getCurrentTime
    tfs <- findTemporaryFilesOlderThen now
    traverse_ deleteTemporaryFile tfs
