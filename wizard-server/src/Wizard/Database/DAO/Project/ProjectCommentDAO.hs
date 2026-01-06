module Wizard.Database.DAO.Project.ProjectCommentDAO where

import Control.Monad.Reader (asks, liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Project.Comment.ProjectComment ()
import Wizard.Database.Mapping.Project.Comment.ProjectCommentThread ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Comment.ProjectComment

entityName = "project_comment"

insertProjectComment :: ProjectComment -> AppContextM Int64
insertProjectComment = createInsertFn entityName

insertProjectThreadAndComment :: ProjectCommentThread -> ProjectComment -> AppContextM Int64
insertProjectThreadAndComment thread comment = do
  let sql =
        fromString $
          f'
            "BEGIN TRANSACTION; \
            \INSERT INTO %s VALUES (%s); \
            \INSERT INTO %s VALUES (%s); \
            \COMMIT;"
            ["project_comment_thread", generateQuestionMarks' thread, entityName, generateQuestionMarks' comment]
  let params = toRow thread ++ toRow comment
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

insertProjectThreadAndComment' :: ProjectCommentThread -> ProjectComment -> AppContextM Int64
insertProjectThreadAndComment' thread comment = do
  let sql =
        fromString $
          f'
            "INSERT INTO %s VALUES (%s); \
            \INSERT INTO %s VALUES (%s); "
            ["project_comment_thread", generateQuestionMarks' thread, entityName, generateQuestionMarks' comment]
  let params = toRow thread ++ toRow comment
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateProjectCommentById :: ProjectComment -> AppContextM ProjectComment
updateProjectCommentById entity = do
  tenantUuid <- asks currentTenantUuid
  now <- liftIO getCurrentTime
  let updatedEntity = entity {updatedAt = now} :: ProjectComment
  let sql =
        fromString
          "UPDATE project_comment SET uuid = ?, text = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow updatedEntity ++ [toField updatedEntity.uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedEntity

updateProjectCommentTextById :: U.UUID -> String -> AppContextM Int64
updateProjectCommentTextById uuid text = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE project_comment SET text = ?, updated_at = now() WHERE uuid = ? AND tenant_uuid = ?"
  let params = [toField text, toField uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectComments :: AppContextM Int64
deleteProjectComments = createDeleteEntitiesFn entityName

deleteProjectCommentsByThreadUuid :: U.UUID -> AppContextM Int64
deleteProjectCommentsByThreadUuid threadUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [("comment_thread_uuid", U.toString threadUuid), ("tenant_uuid", U.toString tenantUuid)]

deleteProjectCommentById :: U.UUID -> AppContextM Int64
deleteProjectCommentById uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [("uuid", U.toString uuid), ("tenant_uuid", U.toString tenantUuid)]
