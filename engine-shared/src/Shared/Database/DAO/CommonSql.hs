module Shared.Database.DAO.CommonSql where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import Data.Pool
import Data.String
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Database.PostgreSQL.Simple as PostgresTransaction
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Localization.Messages.Public
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Util.Logger

runDB :: (MonadReader s m, HasDbPool' s, MonadIO m) => (Connection -> IO b) -> m b
runDB action = do
  context <- ask
  let dbPool = context ^. dbPool'
  liftIO $ withResource dbPool action

runRawDB :: (MonadReader s m, HasDbPool' s, MonadIO m) => (LibPQ.Connection -> IO b) -> m b
runRawDB action = do
  context <- ask
  let dbPool = context ^. dbPool'
  liftIO $ withResource dbPool (`withConnection` action)

runInTransaction :: (MonadReader s m, MonadLogger m, MonadIO m, MonadError e m, HasDbPool' s) => m a -> m a
runInTransaction action = do
  status <- runRawDB LibPQ.transactionStatus
  case status of
    LibPQ.TransInTrans -> do
      logInfo _CMP_DATABASE "Ensuring transaction"
      action
    _ -> do
      runDB PostgresTransaction.begin
      logInfo _CMP_DATABASE "Transaction started"
      result <- catchError action handleError
      runDB PostgresTransaction.commit
      logInfo _CMP_DATABASE "Transaction commited"
      return result
  where
    handleError error = do
      runDB PostgresTransaction.rollback
      logInfo _CMP_DATABASE "Transaction rollback"
      throwError error

createFindEntitiesFn ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m, FromRow entity, FromRow entity)
  => String
  -> m [entity]
createFindEntitiesFn entityName = do
  let sql = f' "SELECT * FROM %s" [entityName]
  logInfo _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  runDB action

createFindEntitiesByFn ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m, FromRow entity, FromRow entity)
  => String
  -> [(String, String)]
  -> m [entity]
createFindEntitiesByFn entityName [] = createFindEntitiesFn entityName
createFindEntitiesByFn entityName queryParams = do
  let sql = f' "SELECT * FROM %s WHERE %s" [entityName, mapToDBQueryParams queryParams]
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) (fmap snd queryParams)
  runDB action

createFindEntityByFn ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m, FromRow entity)
  => String
  -> String
  -> String
  -> m entity
createFindEntityByFn entityName paramName paramValue = do
  let sql = f' "SELECT * FROM %s WHERE %s = ?" [entityName, paramName]
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [paramValue]
  entities <- runDB action
  case entities of
    [] -> throwError $ NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND entityName paramValue)
    [entity] -> return entity
    _ ->
      throwError $
      GeneralServerError
        (f' "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)" [entityName, paramValue])

createFindEntityByFn' ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m, FromRow entity)
  => String
  -> String
  -> String
  -> m (Maybe entity)
createFindEntityByFn' entityName paramName paramValue = do
  let sql = f' "SELECT * FROM %s WHERE %s = ?" [entityName, paramName]
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [paramValue]
  entities <- runDB action
  case entities of
    [] -> return Nothing
    [entity] -> return . Just $ entity
    _ ->
      throwError $
      GeneralServerError
        (f' "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)" [entityName, paramValue])

createInsertFn :: (ToRow q, MonadLogger m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> q -> m Int64
createInsertFn entityName entity = do
  let questionMarks = generateQuestionMarks entity
  let sql = f' "INSERT INTO %s VALUES %s" [entityName, questionMarks]
  logInfo _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) entity
  runDB action

createDeleteEntitiesFn ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Int64
createDeleteEntitiesFn entityName = do
  let sql = f' "DELETE FROM %s" [entityName]
  logInfo _CMP_DATABASE sql
  let action conn = execute_ conn (fromString sql)
  runDB action

createDeleteEntitiesByFn ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m)
  => String
  -> [(String, String)]
  -> m Int64
createDeleteEntitiesByFn entityName [] = createDeleteEntitiesFn entityName
createDeleteEntitiesByFn entityName queryParams = do
  let sql = f' "DELETE FROM %s WHERE %s" [entityName, mapToDBQueryParams queryParams]
  logInfo _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) (fmap snd queryParams)
  runDB action

createDeleteEntityByFn ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m)
  => String
  -> String
  -> String
  -> m Int64
createDeleteEntityByFn entityName paramName paramValue = do
  let sql = f' "DELETE FROM %s WHERE %s = ?" [entityName, paramName]
  logInfo _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) [paramValue]
  runDB action

createCountFn :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Int
createCountFn entityName = do
  let sql = f' "SELECT COUNT(*) FROM %s" [entityName]
  logInfo _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> throwError $ GeneralServerError (f' "createCountFn: there are no selected rows or more than one" [])

generateQuestionMarks :: ToRow entity => entity -> String
generateQuestionMarks entity =
  let size = length $ toRow entity
      generate :: Int -> String
      generate 0 = ""
      generate 1 = "?"
      generate 2 = "?,?"
      generate x = "?," ++ generate (x - 1)
   in f' "(%s)" [generate size]

mapToDBQueryParams :: [(String, String)] -> String
mapToDBQueryParams [(queryParamKey, queryParamValue)] = queryParamKey ++ " = ? "
mapToDBQueryParams ((queryParamKey, queryParamValue):xs) = queryParamKey ++ " = ? AND " ++ mapToDBQueryParams xs
