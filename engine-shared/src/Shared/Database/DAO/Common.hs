module Shared.Database.DAO.Common where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import Data.Maybe
import Data.Pool
import Data.String
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Database.PostgreSQL.Simple as PostgresTransaction
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Util.Logger
import Shared.Util.String (toSnake)

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

runInTransaction ::
     (MonadReader s m, MonadIO m, MonadError e m, HasDbPool' s) => (String -> String -> m ()) -> m a -> m a
runInTransaction logInfoFn action = do
  status <- runRawDB LibPQ.transactionStatus
  case status of
    LibPQ.TransInTrans -> do
      logInfoFn _CMP_DATABASE "Ensuring transaction"
      action
    _ -> do
      runDB PostgresTransaction.begin
      logInfoFn _CMP_DATABASE "Transaction started"
      result <- catchError action handleError
      runDB PostgresTransaction.commit
      logInfoFn _CMP_DATABASE "Transaction commited"
      return result
  where
    handleError error = do
      runDB PostgresTransaction.rollback
      logInfoFn _CMP_DATABASE "Transaction rollback"
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
  let sql = f' "SELECT * FROM %s WHERE %s" [entityName, mapToDBQuerySql queryParams]
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

createFindEntitiesPageableQuerySortFn ::
     (MonadReader s m, FromRow entity, ToRow q, MonadIO m, MonadError AppError m, MonadLogger m, HasDbPool' s)
  => String
  -> String
  -> Pageable
  -> [Sort]
  -> String
  -> String
  -> q
  -> m (Page entity)
createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort fields condition conditionParams
  -- 1. Prepare variables
 = do
  let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
  -- 2. Get total count
  count <- createCountByFn entityName condition conditionParams
  -- 3. Get entities
  let sql =
        f'
          "SELECT %s FROM %s WHERE %s %s OFFSET %s LIMIT %s"
          [fields, entityName, condition, mapSort sort, show skip, show sizeI]
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) conditionParams
  entities <- runDB action
  -- 4. Constructor response
  let metadata =
        PageMetadata
          { _pageMetadataSize = sizeI
          , _pageMetadataTotalElements = count
          , _pageMetadataTotalPages = computeTotalPage count sizeI
          , _pageMetadataNumber = pageI
          }
  return $ Page pageLabel metadata entities

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
  let sql = f' "DELETE FROM %s WHERE %s" [entityName, mapToDBQuerySql queryParams]
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

createCountByFn ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m, ToRow q)
  => String
  -> String
  -> q
  -> m Int
createCountByFn entityName condition queryParams = do
  let sql = f' "SELECT COUNT(*) FROM %s WHERE %s" [entityName, condition]
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) queryParams
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

generateQuestionMarks :: ToRow entity => entity -> String
generateQuestionMarks entity =
  let size = length $ toRow entity
      generate :: Int -> String
      generate 0 = ""
      generate 1 = "?"
      generate 2 = "?,?"
      generate x = "?," ++ generate (x - 1)
   in f' "(%s)" [generate size]

regex :: Maybe String -> String
regex mQuery = ".*" ++ fromMaybe "" mQuery ++ ".*"

mapToDBQuerySql :: [(String, String)] -> String
mapToDBQuerySql [(queryParamKey, queryParamValue)] = queryParamKey ++ " = ? "
mapToDBQuerySql ((queryParamKey, queryParamValue):xs) = queryParamKey ++ " = ? AND " ++ mapToDBQuerySql xs

mapToDBCoordinatesSql :: String -> Maybe String -> Maybe String -> String
mapToDBCoordinatesSql entityId (Just orgId) (Just eId) = f' "and organization_id = ? and %s = ? " [entityId]
mapToDBCoordinatesSql entityId (Just orgId) _ = f' "and organization_id = ? " [entityId]
mapToDBCoordinatesSql entityId _ (Just eId) = f' "and %s = ? " [entityId]
mapToDBCoordinatesSql entityId _ _ = ""

mapToDBCoordinatesParams :: Maybe String -> Maybe String -> [String]
mapToDBCoordinatesParams (Just orgId) (Just eId) = [orgId, eId]
mapToDBCoordinatesParams (Just orgId) _ = [orgId]
mapToDBCoordinatesParams _ (Just eId) = [eId]
mapToDBCoordinatesParams _ _ = []

mapSort :: [Sort] -> String
mapSort [] = ""
mapSort xs = "ORDER BY " ++ createRecord xs
  where
    createRecord [sort] = create sort
    createRecord (sort:xs) = create sort ++ ", " ++ mapSort xs
    create (Sort name order) =
      case order of
        Ascending -> f' "%s asc " [toSnake name]
        Descending -> f' "%s desc " [toSnake name]

preparePaginationVariables :: Pageable -> (Int, Int, Int, Int)
preparePaginationVariables pageable =
  let sizeI = abs . fromMaybe 20 $ pageable ^. size
      pageI = abs . fromMaybe 0 $ pageable ^. page
      skip = fromIntegral $ pageI * sizeI
      limit = fromIntegral sizeI
   in (sizeI, pageI, skip, limit)

computeTotalPage :: Int -> Int -> Int
computeTotalPage 0 0 = 0
computeTotalPage _ 0 = 1
computeTotalPage count size = ceiling $ fromIntegral count / fromIntegral size
