module Shared.Common.Database.DAO.Common where

import Control.Exception
import Control.Monad (when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (ask, liftIO)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import Data.Maybe
import Data.Pool
import Data.String
import qualified Data.UUID as U
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple as PostgresTransaction
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import GHC.Int

import Shared.Common.Localization.Messages.Internal
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Error.Error
import Shared.Common.Util.ByteString (toByteString)
import Shared.Common.Util.Logger
import Shared.Common.Util.String (replace, toSnake, trim)

runDB :: AppContextC s sc m => (Connection -> IO b) -> m b
runDB action = do
  context <- ask
  case context.dbConnection' of
    Just dbConnection -> handleSqlErrorIfPresent $ action dbConnection
    Nothing -> handleSqlErrorIfPresent $ withResource context.dbPool' action

runDBImmediately :: AppContextC s sc m => (Connection -> IO b) -> m b
runDBImmediately action = do
  context <- ask
  handleSqlErrorIfPresent $ withResource context.dbPool' action

runRawDB :: AppContextC s sc m => (LibPQ.Connection -> IO b) -> m b
runRawDB action = do
  context <- ask
  case context.dbConnection' of
    Just dbConnection -> handleSqlErrorIfPresent $ dbConnection `withConnection` action
    Nothing -> handleSqlErrorIfPresent $ withResource context.dbPool' (`withConnection` action)

handleSqlErrorIfPresent :: AppContextC s sc m => IO b -> m b
handleSqlErrorIfPresent eResultIO = do
  context <- ask
  eResult <- liftIO . try $ eResultIO
  case eResult of
    Right result -> return result
    Left ex@SqlError {..} ->
      case ex.sqlState of
        "23505" -> do
          logWarnI _CMP_DATABASE $ f' "Unique constraint violation: %s" [show ex]
          let msg =
                if context.serverConfig'.logging'.databaseDebug
                  then _ERROR_DATABASE__UNIQUE_CONSTRAINT_VIOLATION_WITH_ERROR (BS.unpack ex.sqlErrorMsg) (BS.unpack ex.sqlErrorDetail)
                  else _ERROR_DATABASE__UNIQUE_CONSTRAINT_VIOLATION
          throwError . UserError $ msg
        _ -> throw ex

runOneEntityDB :: AppContextC s sc m => String -> (Connection -> IO [b]) -> [(String, String)] -> m b
runOneEntityDB entityName action queryParams = do
  entities <- runDB action
  case entities of
    [] -> throwError $ NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND entityName queryParams)
    [entity] -> return entity
    _ ->
      throwError $
        GeneralServerError
          ( f'
              "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)"
              [entityName, show (fmap snd queryParams)]
          )

runOneEntityDB' :: AppContextC s sc m => String -> (Connection -> IO [b]) -> [(String, String)] -> m (Maybe b)
runOneEntityDB' entityName action queryParams = do
  entities <- runDB action
  case entities of
    [] -> return Nothing
    [entity] -> return . Just $ entity
    _ ->
      throwError $
        GeneralServerError
          ( f'
              "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)"
              [entityName, show (fmap snd queryParams)]
          )

logQuery :: (AppContextC s sc m, ToRow q) => Query -> q -> m ()
logQuery sql params = do
  context <- ask
  exploded <-
    case context.dbConnection' of
      Just dbConnection -> liftIO $ formatQuery dbConnection sql params
      Nothing -> liftIO $ withResource context.dbPool' (\c -> formatQuery c sql params)
  let formatted = trim . BS.unpack $ exploded
  logInfoI _CMP_DATABASE formatted

logInsertAndUpdate :: (AppContextC s sc m, ToRow q) => Query -> q -> m ()
logInsertAndUpdate sql params = do
  context <- ask
  let sqlTemplate = replace "?" "%s" . trim . BS.unpack . fromQuery $ sql
  parameters <- liftIO $
    case context.dbConnection' of
      Just conn -> createParameters conn sql params
      Nothing -> withResource context.dbPool' (\conn -> createParameters conn sql params)
  logInfoI _CMP_DATABASE $ f' sqlTemplate (fmap showParameter parameters)
  where
    createParameters conn sql params =
      let actions = toRow params
       in traverse (buildAction conn sql actions) actions
    cutParameter param =
      if length param > 50
        then take 50 param ++ "...'"
        else param
    showParameter = cutParameter . BSL.unpack . BSB.toLazyByteString

runInTransaction :: AppContextC s sc m => (String -> String -> m ()) -> (String -> String -> m ()) -> m a -> m a
runInTransaction logInfoFn logWarnFn action = do
  context <- ask
  when
    (isNothing context.dbConnection')
    (throwError $ GeneralServerError _ERROR_DATABASE__TRANSACTION_REQUIRED_DB_CONN)
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
      logWarnFn _CMP_DATABASE (show error)
      throwError error

createFindEntitiesFn :: (AppContextC s sc m, FromRow entity) => String -> m [entity]
createFindEntitiesFn = createFindEntitiesWithFieldsFn "*"

createFindEntitiesWithFieldsFn :: (AppContextC s sc m, FromRow entity) => String -> String -> m [entity]
createFindEntitiesWithFieldsFn fields entityName = do
  let sql = f' "SELECT %s FROM %s" [fields, entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

createFindEntitiesSortedFn :: (AppContextC s sc m, FromRow entity) => String -> [Sort] -> m [entity]
createFindEntitiesSortedFn = createFindEntitiesWithFieldsSortedFn "*"

createFindEntitiesWithFieldsSortedFn :: (AppContextC s sc m, FromRow entity) => String -> String -> [Sort] -> m [entity]
createFindEntitiesWithFieldsSortedFn fields entityName sort = do
  let sql = f' "SELECT %s FROM %s %s" [fields, entityName, mapSort sort]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

createFindEntitiesByFn :: (AppContextC s sc m, FromRow entity) => String -> [(String, String)] -> m [entity]
createFindEntitiesByFn entityName [] = createFindEntitiesFn entityName
createFindEntitiesByFn entityName queryParams = do
  let sql = fromString $ f' "SELECT * FROM %s WHERE %s" [entityName, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

createFindEntitiesWithFieldsByFn :: (AppContextC s sc m, FromRow entity) => String -> String -> [(String, String)] -> m [entity]
createFindEntitiesWithFieldsByFn fields entityName [] = createFindEntitiesWithFieldsFn fields entityName
createFindEntitiesWithFieldsByFn fields entityName queryParams = do
  let sql = fromString $ f' "SELECT %s FROM %s WHERE %s" [fields, entityName, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

createFindEntitiesBySortedFn :: (AppContextC s sc m, FromRow entity) => String -> [(String, String)] -> [Sort] -> m [entity]
createFindEntitiesBySortedFn = createFindEntitiesWithFieldsBySortedFn "*"

createFindEntitiesWithFieldsBySortedFn :: (AppContextC s sc m, FromRow entity) => String -> String -> [(String, String)] -> [Sort] -> m [entity]
createFindEntitiesWithFieldsBySortedFn fields entityName [] sort = createFindEntitiesWithFieldsSortedFn fields entityName sort
createFindEntitiesWithFieldsBySortedFn fields entityName queryParams sort = do
  let sql = fromString $ f' "SELECT %s FROM %s WHERE %s %s" [fields, entityName, mapToDBQuerySql queryParams, mapSort sort]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

createFindEntityByFn :: (AppContextC s sc m, FromRow entity) => String -> [(String, String)] -> m entity
createFindEntityByFn = createFindEntityWithFieldsByFn "*" False

createFindEntityWithFieldsByFn :: (AppContextC s sc m, FromRow entity) => String -> Bool -> String -> [(String, String)] -> m entity
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
          ( f'
              "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)"
              [entityName, show (fmap snd queryParams)]
          )

createFindEntityByFn' :: (AppContextC s sc m, FromRow entity) => String -> [(String, String)] -> m (Maybe entity)
createFindEntityByFn' = createFindEntityWithFieldsByFn' "*"

createFindEntityWithFieldsByFn' :: (AppContextC s sc m, FromRow entity) => String -> String -> [(String, String)] -> m (Maybe entity)
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
          ( f'
              "createFindEntityByFn: find more entities found than one (entity: %s, param: %s)"
              [entityName, show (fmap snd queryParams)]
          )

createFindEntitiesPageableQuerySortFn
  :: (AppContextC s sc m, ToRow q, FromRow entity)
  => String
  -> String
  -> Pageable
  -> [Sort]
  -> String
  -> String
  -> q
  -> m (Page entity)
createFindEntitiesPageableQuerySortFn entityName pageLabel pageable sort fields condition conditionParams =
  -- 1. Prepare variables
  do
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
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

createFindEntitiesPageableQuerySortWithComputedEntityFn
  :: (AppContextC s sc m, ToRow q, FromRow entity)
  => String
  -> String
  -> Pageable
  -> [Sort]
  -> String
  -> String
  -> q
  -> m (Page entity)
createFindEntitiesPageableQuerySortWithComputedEntityFn withSelect pageLabel pageable sort fields condition conditionParams =
  -- 1. Prepare variables
  do
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByWithComputedEntityFn withSelect condition conditionParams
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "WITH computed_entity AS (%s) SELECT %s FROM computed_entity %s %s OFFSET %s LIMIT %s"
              [withSelect, fields, condition, mapSort sort, show skip, show sizeI]
    logQuery sql conditionParams
    let action conn = query conn sql conditionParams
    entities <- runDB action
    -- 4. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

createFindColumnBySqlPageFn
  :: (AppContextC s sc m, FromField entity)
  => String
  -> Pageable
  -> Query
  -> [String]
  -> Int
  -> m (Page entity)
createFindColumnBySqlPageFn pageLabel pageable sql params count =
  -- 1. Prepare variables
  do
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Perform query
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 3. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata (concat entities)

createFindEntitiesInFn :: (AppContextC s sc m, FromRow entity) => String -> U.UUID -> String -> [String] -> m [entity]
createFindEntitiesInFn entityName tenantUuid paramName params = do
  let sql = fromString $ f' "SELECT * FROM %s WHERE tenant_uuid = '%s' AND %s IN (%s)" [entityName, U.toString tenantUuid, paramName, generateQuestionMarks params]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

createInsertFn :: (AppContextC s sc m, ToRow entity) => String -> entity -> m Int64
createInsertFn entityName entity = do
  let sql = fromString $ f' "INSERT INTO %s VALUES (%s)" [entityName, generateQuestionMarks' entity]
  let params = entity
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

createInsertWithoutTransactionFn :: (AppContextC s sc m, ToRow entity) => String -> entity -> m Int64
createInsertWithoutTransactionFn entityName entity = do
  let sql = fromString $ f' "INSERT INTO %s VALUES (%s)" [entityName, generateQuestionMarks' entity]
  let params = entity
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDBImmediately action

createDeleteEntitiesFn :: AppContextC s sc m => String -> m Int64
createDeleteEntitiesFn entityName = do
  let sql = f' "DELETE FROM %s" [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

createDeleteEntitiesByFn :: AppContextC s sc m => String -> [(String, String)] -> m Int64
createDeleteEntitiesByFn entityName [] = createDeleteEntitiesFn entityName
createDeleteEntitiesByFn entityName queryParams = do
  let sql = fromString $ f' "DELETE FROM %s WHERE %s" [entityName, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

createDeleteEntityWhereInFn :: AppContextC s sc m => String -> String -> [String] -> m Int64
createDeleteEntityWhereInFn entityName key params = do
  let sql = fromString $ f' "DELETE FROM %s WHERE %s IN (%s)" [entityName, key, generateQuestionMarks params]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

createDeleteEntityByFn :: AppContextC s sc m => String -> [(String, String)] -> m Int64
createDeleteEntityByFn entityName queryParams = do
  let sql = fromString $ f' "DELETE FROM %s WHERE %s" [entityName, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

createCountFn :: AppContextC s sc m => String -> m Int64
createCountFn entityName = do
  let sql = f' "SELECT COUNT(*) FROM %s" [entityName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> throwError $ GeneralServerError (f' "createCountFn: there are no selected rows or more than one" [])

createCountByFn :: (AppContextC s sc m, ToRow q) => String -> String -> q -> m Int
createCountByFn entityName condition queryParams = do
  let sql = fromString $ f' "SELECT COUNT(*) FROM %s %s" [entityName, condition]
  let params = queryParams
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

createCountWithSqlFn :: AppContextC s sc m => Query -> [String] -> m Int
createCountWithSqlFn sql params = do
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

createCountByWithComputedEntityFn :: (AppContextC s sc m, ToRow q) => String -> String -> q -> m Int
createCountByWithComputedEntityFn withSelect condition queryParams = do
  let sql = fromString $ f' "WITH computed_entity AS (%s) SELECT COUNT(*) FROM computed_entity %s" [withSelect, condition]
  let params = queryParams
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

createSumByFn :: (AppContextC s sc m, ToRow q) => String -> String -> String -> q -> m Int64
createSumByFn entityName field condition queryParams = do
  let sql = fromString $ f' "SELECT COALESCE(SUM(%s)::bigint, 0) FROM %s %s" [field, entityName, condition]
  let params = queryParams
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

generateQuestionMarks :: [a] -> String
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

generateQuestionMarksForEntities :: ToRow entity => [entity] -> String
generateQuestionMarksForEntities entities =
  let oneRow entity = "(" ++ generateQuestionMarks' entity ++ ")"
   in L.intercalate "," . fmap oneRow $ entities

regex :: String -> String
regex query = ".*" ++ query ++ ".*"

regexM :: Maybe String -> String
regexM query = ".*" ++ fromMaybe "" query ++ ".*"

mapToDBQuerySql :: [(String, String)] -> String
mapToDBQuerySql [] = ""
mapToDBQuerySql [(queryParamKey, queryParamValue)] = queryParamKey ++ " = ? "
mapToDBQuerySql ((queryParamKey, queryParamValue) : xs) = queryParamKey ++ " = ? AND " ++ mapToDBQuerySql xs

mapToDBCoordinatesSql :: String -> String -> Maybe String -> Maybe String -> String
mapToDBCoordinatesSql entityName entityId (Just orgId) (Just eId) =
  f' "and %s.organization_id = ? and %s.%s = ? " [entityName, entityName, entityId]
mapToDBCoordinatesSql entityName entityId (Just orgId) _ = f' "and %s.organization_id = ? " [entityName]
mapToDBCoordinatesSql entityName entityId _ (Just eId) = f' "and %s.%s = ? " [entityName, entityId]
mapToDBCoordinatesSql _ _ _ _ = ""

mapToDBCoordinatesParams :: Maybe String -> Maybe String -> [String]
mapToDBCoordinatesParams (Just orgId) (Just eId) = [orgId, eId]
mapToDBCoordinatesParams (Just orgId) _ = [orgId]
mapToDBCoordinatesParams _ (Just eId) = [eId]
mapToDBCoordinatesParams _ _ = []

mapSort :: [Sort] -> String
mapSort [] = ""
mapSort xs = "ORDER BY " ++ createRecord xs
  where
    createRecord [] = ""
    createRecord [sort] = create sort
    createRecord (sort : xs) = create sort ++ ", " ++ createRecord xs
    create (Sort name order) =
      case order of
        Ascending -> f' "%s asc " [toSnake name]
        Descending -> f' "%s desc " [toSnake name]

mapSortWithPrefix :: String -> [Sort] -> String
mapSortWithPrefix _ [] = ""
mapSortWithPrefix prefix xs = "ORDER BY " ++ createRecord xs
  where
    createRecord [] = ""
    createRecord [sort] = create sort
    createRecord (sort : xs) = create sort ++ ", " ++ mapSort xs
    create (Sort name order) =
      case order of
        Ascending -> f' "%s.%s asc " [prefix, toSnake name]
        Descending -> f' "%s.%s desc " [prefix, toSnake name]

mapSortWithCustomMapping :: [Sort] -> [(String, String)] -> String
mapSortWithCustomMapping [] customMapping = ""
mapSortWithCustomMapping xs customMapping = "ORDER BY " ++ createRecord xs
  where
    createRecord [] = ""
    createRecord [sort] = create sort
    createRecord (sort : xs) = create sort ++ ", " ++ mapSortWithCustomMapping xs customMapping
    create (Sort name order) =
      let sanitize name =
            case L.find (\(cmName, value) -> cmName == name) customMapping of
              Just (cmName, cmValue) -> cmValue
              Nothing -> toSnake name
       in case order of
            Ascending -> f' "%s asc " [sanitize name]
            Descending -> f' "%s desc " [sanitize name]

preparePaginationVariables :: Pageable -> (Int, Int, Int, Int)
preparePaginationVariables pageable =
  let sizeI = abs . fromMaybe 20 $ pageable.size
      pageI = abs . fromMaybe 0 $ pageable.page
      skip = fromIntegral $ pageI * sizeI
      limit = fromIntegral sizeI
   in (sizeI, pageI, skip, limit)

computeTotalPage :: Int -> Int -> Int
computeTotalPage 0 0 = 0
computeTotalPage _ 0 = 1
computeTotalPage count size = ceiling $ fromIntegral count / fromIntegral size

tenantQueryUuid :: U.UUID -> (String, String)
tenantQueryUuid tenantUuid = ("tenant_uuid", U.toString tenantUuid)

tenantCondition :: String
tenantCondition = "WHERE tenant_uuid = ?"

showAction :: Action -> String
showAction (Plain a) = BS.unpack . toByteString $ a
showAction (Escape a) = BS.unpack a
showAction (EscapeByteA a) = BS.unpack a
showAction (EscapeIdentifier a) = BS.unpack a
showAction (Many xs) = show . fmap showAction $ xs

isAndOperator :: Maybe String -> Bool
isAndOperator operator =
  operator == Just "" || operator == Just "and" || operator == Just "AND" || operator == Just "And"
