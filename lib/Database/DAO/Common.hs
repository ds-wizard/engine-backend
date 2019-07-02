module Database.DAO.Common where

import Control.Monad.Reader (asks, liftIO)
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.UUID as U
import Database.MongoDB
       ((=:), delete, deleteOne, fetch, find, findOne, insert, merge,
        modify, rest, save, select)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Localization
import Model.Context.AppContext
import Model.Error.Error

runDB action = do
  dbPool <- asks _appContextPool
  liftIO $ runMongoDBPoolDef action dbPool

deserializeEntities :: (FromBSON a) => [Document] -> Either AppError [a]
deserializeEntities entitiesS = do
  let maybeEntities = fromBSON <$> entitiesS
  let entities = catMaybes maybeEntities
  if length maybeEntities == length entities
    then Right entities
    else Left . DatabaseError $ _ERROR_DATABASE__DESERIALIZATION_FAILED

deserializeMaybeEntity :: (FromBSON a) => String -> String -> Maybe Document -> Either AppError a
deserializeMaybeEntity entityName identificator mEntityS =
  case mEntityS of
    Just entityS -> do
      let mEntity = fromBSON entityS
      case mEntity of
        Just entity -> Right entity
        Nothing -> Left . DatabaseError $ _ERROR_DATABASE__DESERIALIZATION_FAILED
    Nothing -> Left . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND entityName identificator

createFindEntitiesFn collection = do
  let action = rest =<< find (select [] collection)
  entitiesS <- runDB action
  return . deserializeEntities $ entitiesS

createFindEntitiesByFn collection queryParams = do
  let action = rest =<< find (select queryParams collection)
  entitiesS <- runDB action
  return . deserializeEntities $ entitiesS

createFindEntityByFn collection entityName paramName paramValue = do
  let action = findOne $ select [paramName =: paramValue] collection
  maybeEntityS <- runDB action
  return . deserializeMaybeEntity entityName paramValue $ maybeEntityS

createInsertFn collection entity = do
  let action = insert collection (toBSON entity)
  runDB action

createUpdateByFn collection paramName paramValue entity = do
  let action = fetch (select [paramName =: paramValue] collection) >>= save collection . merge (toBSON entity)
  runDB action

createPartialUpdateByFn collection queryParams updatedFields = do
  let action = modify (select queryParams collection) ["$set" =: updatedFields]
  runDB action

createPartialUpdateByFn' collection paramName paramValue fieldName fieldValue =
  createPartialUpdateByFn collection [paramName =: paramValue] [fieldName =: fieldValue]

createDeleteEntitiesFn collection = do
  let action = delete $ select [] collection
  runDB action

createDeleteEntitiesByFn collection queryParams = do
  let action = delete $ select queryParams collection
  runDB action

createDeleteEntityByFn collection paramName paramValue = do
  let action = deleteOne $ select [paramName =: paramValue] collection
  runDB action

mapToDBQueryParams queryParams = fmap go queryParams
  where
    go :: (Text, Text) -> Field
    go (p, v) = p =: v

instance Val LT.Text where
  val = String . LT.toStrict
  cast' (String x) = Just . LT.fromStrict $ x
  cast' (Sym (Symbol x)) = Just . LT.fromStrict $ x
  cast' _ = Nothing

instance Val U.UUID where
  val = String . U.toText
  cast' (String x) = U.fromText x
  cast' (Sym (Symbol x)) = U.fromText x
  cast' _ = Nothing
