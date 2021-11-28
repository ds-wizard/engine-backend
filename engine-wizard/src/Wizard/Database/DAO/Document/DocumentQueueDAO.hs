module Wizard.Database.DAO.Document.DocumentQueueDAO where

import Control.Monad.Except (throwError)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Model.Error.Error
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Document.DocumentQueue ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Document.DocumentQueue
import Wizard.Util.Logger

entityName = "document_queue"

insertDocumentQueue :: DocumentQueue -> AppContextM Int
insertDocumentQueue entity = do
  let questionMarks = generateQuestionMarks entity
  let sql =
        fromString $
        f'
          "INSERT INTO %s (document_uuid, document_context, created_by, created_at, app_uuid) VALUES (?, ?, ?, ?, ?) RETURNING id"
          [entityName, questionMarks]
  let params = entity
  logQuery sql params
  let action conn = query conn sql entity
  result <- runDB action
  case result of
    [dId] -> return . fromOnly $ dId
    _ -> throwError $ GeneralServerError (f' "insert: returning id failed" [])

notifyDocumentQueue :: Int -> AppContextM Int64
notifyDocumentQueue dqId = do
  let sql = f' "NOTIFY document_queue_channel, '%s'" [show dqId]
  logInfo _CMP_DATABASE sql
  let action conn = execute_ conn (fromString sql)
  runDB action

deleteDocumentQueues :: AppContextM Int64
deleteDocumentQueues = createDeleteEntitiesFn entityName
