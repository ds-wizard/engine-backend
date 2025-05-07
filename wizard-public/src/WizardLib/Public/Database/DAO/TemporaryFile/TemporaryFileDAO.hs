module WizardLib.Public.Database.DAO.TemporaryFile.TemporaryFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import WizardLib.Public.Database.Mapping.TemporaryFile.TemporaryFile ()
import WizardLib.Public.Model.TemporaryFile.TemporaryFile

entityName = "temporary_file"

findTemporaryFiles :: AppContextC s sc m => m [TemporaryFile]
findTemporaryFiles = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findTemporaryFilesOlderThen :: AppContextC s sc m => UTCTime -> m [TemporaryFile]
findTemporaryFilesOlderThen date = do
  let sql = fromString $ f' "SELECT * FROM %s WHERE expires_at < ? " [entityName]
  let params = [toField date]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertTemporaryFile :: AppContextC s sc m => TemporaryFile -> m Int64
insertTemporaryFile = createInsertFn entityName

deleteTemporaryFiles :: AppContextC s sc m => m Int64
deleteTemporaryFiles = createDeleteEntitiesFn entityName

deleteTemporaryFileByUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteTemporaryFileByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
