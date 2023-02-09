module Wizard.Service.TemporaryFile.TemporaryFileService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import Data.Time
import Network.URI.Encode (encode)

import Shared.Util.String
import Shared.Util.Uuid
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.TemporaryFile.TemporaryFileDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.TemporaryFile.TemporaryFile
import Wizard.S3.TemporaryFile.TemporaryFileS3
import Wizard.Service.TemporaryFile.TemporaryFileMapper

createTemporaryFile :: String -> String -> BSL.ByteString -> AppContextM String
createTemporaryFile fileName contentType content = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    appUuid <- asks currentAppUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    let expirationInSeconds = 60
    let escapedFileName = filter isLetterOrDotOrDashOrUnderscore fileName
    let tf = toTemporaryFile uuid escapedFileName contentType expirationInSeconds appUuid currentUser.uuid now
    insertTemporaryFile tf
    let contentDisposition = f' "attachment;filename=\"%s\"" [fileName]
    putTemporaryFile tf.uuid escapedFileName tf.contentType contentDisposition (BSL.toStrict content)
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
