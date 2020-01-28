module Registry.Database.DAO.Common where

import Control.Monad.Except (liftEither, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Database.MongoDB
  ( (=:)
  , count
  , delete
  , deleteOne
  , fetch
  , find
  , findOne
  , insert
  , merge
  , modify
  , rest
  , save
  , select
  )
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Registry.Model.Context.AppContext
import Shared.Localization.Messages.Internal
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

runDB action = do
  dbPool <- asks _appContextPool
  liftIO $ runMongoDBPoolDef action dbPool

deserializeEntities :: (FromBSON a) => [Document] -> Either AppError [a]
deserializeEntities entitiesS = do
  let maybeEntities = fromBSON <$> entitiesS
  let entities = catMaybes maybeEntities
  if length maybeEntities == length entities
    then Right entities
    else Left . GeneralServerError $ _ERROR_DATABASE__DESERIALIZATION_FAILED

deserializeMaybeEntity :: (FromBSON a) => String -> String -> Maybe Document -> Either AppError a
deserializeMaybeEntity entityName identificator mEntityS =
  case mEntityS of
    Just entityS -> do
      let mEntity = fromBSON entityS
      case mEntity of
        Just entity -> Right entity
        Nothing -> Left . GeneralServerError $ _ERROR_DATABASE__DESERIALIZATION_FAILED
    Nothing -> Left . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND entityName identificator

createFindEntitiesFn collection = do
  let action = rest =<< find (select [] collection)
  entitiesS <- runDB action
  liftEither . deserializeEntities $ entitiesS

createFindEntitiesByFn collection queryParams = do
  let action = rest =<< find (select queryParams collection)
  entitiesS <- runDB action
  liftEither . deserializeEntities $ entitiesS

createFindEntityFn collection entityName = do
  let action = findOne $ select [] collection
  maybeEntityS <- runDB action
  liftEither . deserializeMaybeEntity entityName "nothing" $ maybeEntityS

createFindEntityByFn collection entityName paramName paramValue = do
  let action = findOne $ select [paramName =: paramValue] collection
  maybeEntityS <- runDB action
  liftEither . deserializeMaybeEntity entityName paramValue $ maybeEntityS

createFindEntityByFn' collection entityName paramName paramValue = do
  let action = findOne $ select [paramName =: paramValue] collection
  maybeEntityS <- runDB action
  case deserializeMaybeEntity entityName paramValue maybeEntityS of
    Right entity -> return (Just entity)
    Left (NotExistsError error) -> return Nothing
    Left error -> throwError error

createInsertFn collection entity = do
  let action = insert collection (toBSON entity)
  runDB action

createUpdateFn collection entity = do
  let action = fetch (select [] collection) >>= save collection . merge (toBSON entity)
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

createCountFn collection = do
  let action = count $ select [] collection
  count <- runDB action
  return . Right $ count

mapToDBQueryParams queryParams = fmap go queryParams
  where
    go :: (String, String) -> Field
    go (p, v) = (T.pack p) =: v

instance Val LT.Text where
  val = String . LT.toStrict
  cast' (String x) = Just . LT.fromStrict $ x
  cast' (Sym (Symbol x)) = Just . LT.fromStrict $ x
  cast' _ = Nothing
