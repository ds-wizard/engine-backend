module Shared.Database.DAO.Common where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.String
import qualified Data.UUID as U
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Database.PostgreSQL.Simple as PostgresTransaction
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.ToField
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
import Shared.Util.ByteString (toByteString)
import Shared.Util.Logger
import Shared.Util.String (toSnake)

runDB ::
     (MonadReader s m, HasDbConnection' s, HasIdentityUuid' s, HasTraceUuid' s, MonadIO m)
  => (Connection -> IO b)
  -> m b
runDB action = do
  context <- ask
  liftIO $ action (context ^. dbConnection')

runRawDB ::
     (MonadReader s m, HasDbConnection' s, HasIdentityUuid' s, HasTraceUuid' s, MonadIO m)
  => (LibPQ.Connection -> IO b)
  -> m b
runRawDB action = do
  context <- ask
  liftIO $ (context ^. dbConnection') `withConnection` action

logQuery ::
     (MonadReader s m, HasDbConnection' s, HasIdentityUuid' s, HasTraceUuid' s, MonadIO m, ToRow q, MonadLogger m)
  => Query
  -> q
  -> m ()
logQuery sql params = do
  context <- ask
  let dbConnection = context ^. dbConnection'
  exploded <- liftIO $ formatQuery dbConnection sql params
  logInfoI _CMP_DATABASE (BS.unpack exploded)

logInsertAndUpdate ::
     (MonadReader s m, HasDbConnection' s, HasIdentityUuid' s, HasTraceUuid' s, MonadIO m, ToRow q, MonadLogger m)
  => Query
  -> q
  -> m ()
logInsertAndUpdate sql params = do
  context <- ask
  let dbConnection = context ^. dbConnection'
  let cut p =
        if length p > 50
          then take 50 p ++ "..."
          else p
  let paramsS = show . fmap (cut . showAction) . toRow $ params
  logInfoI _CMP_DATABASE $ f' "%s  with params %s" [show sql, paramsS]

runInTransaction ::
     ( Show e
     , MonadIO m
     , MonadReader s m
     , MonadIO m
     , MonadError e m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     )
  => (String -> String -> m ())
  -> (String -> String -> m ())
  -> m a
  -> m a
runInTransaction logInfoFn logErrorFn action = do
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
      logInfoFn _CMP_DATABASE "Transaction committed"
      return result
  where
    handleError error = do
      runDB PostgresTransaction.rollback
      logInfoFn _CMP_DATABASE "Transaction rollback"
      logErrorFn _CMP_DATABASE (show error)
      throwError error

createFindEntitiesFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , FromRow entity
     , FromRow entity
     )
  => String
  -> m [entity]
createFindEntitiesFn entityName = do
  let sql = f' "SELECT * FROM %s" [entityName]
  logInfoI _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  runDB action

createFindEntitiesSortedFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , FromRow entity
     , FromRow entity
     )
  => String
  -> [Sort]
  -> m [entity]
createFindEntitiesSortedFn entityName sort = do
  let sql = f' "SELECT * FROM %s %s" [entityName, mapSort sort]
  logInfoI _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  runDB action

createFindEntitiesByFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , FromRow entity
     , FromRow entity
     )
  => String
  -> [(String, String)]
  -> m [entity]
createFindEntitiesByFn entityName [] = createFindEntitiesFn entityName
createFindEntitiesByFn entityName queryParams = do
  let sql = fromString $ f' "SELECT * FROM %s WHERE %s" [entityName, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

createFindEntitiesBySortedFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , FromRow entity
     , FromRow entity
     )
  => String
  -> [(String, String)]
  -> [Sort]
  -> m [entity]
createFindEntitiesBySortedFn entityName [] sort = createFindEntitiesSortedFn entityName sort
createFindEntitiesBySortedFn entityName queryParams sort = do
  let sql = fromString $ f' "SELECT * FROM %s WHERE %s %s" [entityName, mapToDBQuerySql queryParams, mapSort sort]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

createFindEntityByFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , FromRow entity
     )
  => String
  -> [(String, String)]
  -> m entity
createFindEntityByFn = createFindEntityWithFieldsByFn "*" False

createFindEntityWithFieldsByFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , FromRow entity
     )
  => String
  -> Bool
  -> String
  -> [(String, String)]
  -> m entity
createFindEntityWithFieldsByFn fields isForUpdate entityName queryParams = do
  let forUpdate =
        if isForUpdate
          then "FOR UPDATE"
          else ""
  let sql = fromString $ f' "SELECT %s FROM %s WHERE %s %s" [fields, entityName, mapToDBQuerySql queryParams, forUpdate]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  case entities of
    [] -> throwError $ NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND entityName queryParams)
    [entity] -> return entity
    _ ->
      throwError $
      GeneralServerError
        (f'
           "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)"
           [entityName, show (fmap snd queryParams)])

createFindEntityByFn' ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , FromRow entity
     )
  => String
  -> [(String, String)]
  -> m (Maybe entity)
createFindEntityByFn' = createFindEntityWithFieldsByFn' "*"

createFindEntityWithFieldsByFn' ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , FromRow entity
     )
  => String
  -> String
  -> [(String, String)]
  -> m (Maybe entity)
createFindEntityWithFieldsByFn' fields entityName queryParams = do
  let sql = fromString $ f' "SELECT %s FROM %s WHERE %s" [fields, entityName, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  case entities of
    [] -> return Nothing
    [entity] -> return . Just $ entity
    _ ->
      throwError $
      GeneralServerError
        (f'
           "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)"
           [entityName, show (fmap snd queryParams)])

createFindEntitiesPageableQuerySortFn ::
     ( MonadReader s m
     , FromRow entity
     , ToRow q
     , MonadIO m
     , MonadError AppError m
     , MonadLogger m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     )
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
        fromString $
        f'
          "SELECT %s FROM %s %s %s OFFSET %s LIMIT %s"
          [fields, entityName, condition, mapSort sort, show skip, show sizeI]
  logQuery sql conditionParams
  let action conn = query conn sql conditionParams
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

createFindColumnBySqlPageFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , FromField entity
     )
  => String
  -> Pageable
  -> Query
  -> [String]
  -> Int
  -> m (Page entity)
createFindColumnBySqlPageFn pageLabel pageable sql params count
    -- 1. Prepare variables
 = do
  let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Perform query
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
    -- 3. Constructor response
  let metadata =
        PageMetadata
          { _pageMetadataSize = sizeI
          , _pageMetadataTotalElements = count
          , _pageMetadataTotalPages = computeTotalPage count sizeI
          , _pageMetadataNumber = pageI
          }
  return $ Page pageLabel metadata (concat entities)

createInsertFn ::
     (ToRow q, MonadLogger m, MonadReader s m, HasDbConnection' s, HasIdentityUuid' s, HasTraceUuid' s, MonadIO m)
  => String
  -> q
  -> m Int64
createInsertFn entityName entity = do
  let sql = fromString $ f' "INSERT INTO %s VALUES (%s)" [entityName, generateQuestionMarks' entity]
  let params = entity
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

createDeleteEntitiesFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     )
  => String
  -> m Int64
createDeleteEntitiesFn entityName = do
  let sql = f' "DELETE FROM %s" [entityName]
  logInfoI _CMP_DATABASE sql
  let action conn = execute_ conn (fromString sql)
  runDB action

createDeleteEntitiesByFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     )
  => String
  -> [(String, String)]
  -> m Int64
createDeleteEntitiesByFn entityName [] = createDeleteEntitiesFn entityName
createDeleteEntitiesByFn entityName queryParams = do
  let sql = fromString $ f' "DELETE FROM %s WHERE %s" [entityName, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

createDeleteEntityByFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     )
  => String
  -> [(String, String)]
  -> m Int64
createDeleteEntityByFn entityName queryParams = do
  let sql = fromString $ f' "DELETE FROM %s WHERE %s" [entityName, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

createCountFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     )
  => String
  -> m Int
createCountFn entityName = do
  let sql = f' "SELECT COUNT(*) FROM %s" [entityName]
  logInfoI _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> throwError $ GeneralServerError (f' "createCountFn: there are no selected rows or more than one" [])

createCountByFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , ToRow q
     )
  => String
  -> String
  -> q
  -> m Int
createCountByFn entityName condition queryParams = do
  let sql = fromString $ f' "SELECT COUNT(*) FROM %s %s" [entityName, condition]
  let params = queryParams
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

createCountWithSqlFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     )
  => Query
  -> [String]
  -> m Int
createCountWithSqlFn sql params = do
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

createSumByFn ::
     ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasDbConnection' s
     , HasIdentityUuid' s
     , HasTraceUuid' s
     , MonadIO m
     , ToRow q
     )
  => String
  -> String
  -> String
  -> q
  -> m Int64
createSumByFn entityName field condition queryParams = do
  let sql = fromString $ f' "SELECT COALESCE(SUM(%s)::bigint, 0) FROM %s %s" [field, entityName, condition]
  let params = queryParams
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

generateQuestionMarks :: [String] -> String
generateQuestionMarks fields =
  let size = length fields
      generate :: Int -> String
      generate 0 = ""
      generate 1 = "?"
      generate 2 = "?,?"
      generate x = "?," ++ generate (x - 1)
   in f' "%s" [generate size]

generateQuestionMarks' :: ToRow entity => entity -> String
generateQuestionMarks' = generateQuestionMarks . fmap show . toRow

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

mapSortWithPrefix :: String -> [Sort] -> String
mapSortWithPrefix _ [] = ""
mapSortWithPrefix prefix xs = "ORDER BY " ++ createRecord xs
  where
    createRecord [sort] = create sort
    createRecord (sort:xs) = create sort ++ ", " ++ mapSort xs
    create (Sort name order) =
      case order of
        Ascending -> f' "%s.%s asc " [prefix, toSnake name]
        Descending -> f' "%s.%s desc " [prefix, toSnake name]

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

appQueryUuid :: U.UUID -> (String, String)
appQueryUuid appUuid = ("app_uuid", U.toString appUuid)

appQueryString :: String -> (String, String)
appQueryString appUuid = ("app_uuid", appUuid)

appCondition :: String
appCondition = "WHERE app_uuid = ?"

appSelector :: U.UUID -> String
appSelector appUuid = "app_uuid = '" ++ U.toString appUuid ++ "'"

showAction :: Action -> String
showAction (Plain a) = BS.unpack . toByteString $ a
showAction (Escape a) = BS.unpack a
showAction (EscapeByteA a) = BS.unpack a
showAction (EscapeIdentifier a) = BS.unpack a
showAction (Many xs) = show . fmap showAction $ xs

isAndOperator :: Maybe String -> Bool
isAndOperator operator =
  operator == Just "" || operator == Just "and" || operator == Just "AND" || operator == Just "And"
