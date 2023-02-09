module Wizard.Service.TemporaryFile.TemporaryFileService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import Data.Time

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
  uuid <- liftIO generateUuid
  appUuid <- asks currentAppUuid
  currentUser <- getCurrentUser
  now <- liftIO getCurrentTime
  let expirationInSeconds = 60
  let tf = toTemporaryFile uuid fileName contentType expirationInSeconds appUuid currentUser.uuid now
  insertTemporaryFile tf
  putTemporaryFile tf.uuid tf.fileName tf.contentType (BSL.toStrict content)
  presigneGetTemporaryFileUrl tf.uuid tf.fileName expirationInSeconds

deleteTemporaryFile :: TemporaryFile -> AppContextM ()
deleteTemporaryFile tf = do
  deleteTemporaryFileByUuid tf.uuid
  removeTemporaryFile tf.uuid tf.fileName

cleanTemporaryFiles :: AppContextM ()
cleanTemporaryFiles =
  runInTransaction $ do
    now <- liftIO getCurrentTime
    tfs <- findTemporaryFilesOlderThen now
    traverse_ deleteTemporaryFile tfs
