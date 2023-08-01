module Wizard.Database.DAO.TemporaryFile.TemporaryFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.TemporaryFile.TemporaryFile ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.TemporaryFile.TemporaryFile

entityName = "temporary_file"

findTemporaryFiles :: AppContextM [TemporaryFile]
findTemporaryFiles = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findTemporaryFilesOlderThen :: UTCTime -> AppContextM [TemporaryFile]
findTemporaryFilesOlderThen date = do
  let sql = fromString $ f' "SELECT * FROM %s WHERE expires_at < ? " [entityName]
  let params = [toField date]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertTemporaryFile :: TemporaryFile -> AppContextM Int64
insertTemporaryFile = createInsertFn entityName

deleteTemporaryFiles :: AppContextM Int64
deleteTemporaryFiles = createDeleteEntitiesFn entityName

deleteTemporaryFileByUuid :: U.UUID -> AppContextM Int64
deleteTemporaryFileByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
