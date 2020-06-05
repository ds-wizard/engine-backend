module Shared.Database.DAO.Common where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, liftIO)
import Data.Bson
import Data.Bson.Generic
import qualified Data.ByteString.Char8 as BS
import Data.Conduit (($$), (.|), runConduit, yield)
import qualified Data.Conduit.List as CL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Database.MongoDB
  ( Query(..)
  , (=:)
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
  , runCommand
  , save
  , select
  , sort
  )
import Database.MongoDB.GridFS (deleteFile, fetchFile, findFile, openBucket, sinkFile, sourceFile)
import Database.MongoDB.Query (Action)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import LensesConfig
import Shared.Localization.Messages.Internal
import Shared.Localization.Messages.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Sort
import Shared.Model.Context.AppContextLenses
import Shared.Model.Error.Error

runDB :: (MonadReader s m, HasPool' s, MonadIO m) => Action IO b -> m b
runDB action = do
  context <- ask
  let dbPool = context ^. pool'
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

createFindEntitiesPageableQuerySortFn collection query pageable sort
  -- 1. Prepare variables
 = do
  let sizeI = abs . fromMaybe 0 $ pageable ^. size
  let pageI = abs . fromMaybe 0 $ pageable ^. page
  let skip = fromIntegral $ pageI * sizeI
  let limit = fromIntegral sizeI
  -- 2. Get total count
  let countFn = createCountQueryFn collection query
  count <- countFn
  -- 3. Get entities
  let action = rest =<< find ((select query collection) {skip = skip, limit = limit, sort = mapSort sort})
  entitiesS <- runDB action
  entities <- liftEither . deserializeEntities $ entitiesS
  -- 4. Constructor response
  let metadata =
        PageMetadata
          { _pageMetadataSize = sizeI
          , _pageMetadataTotalElements = count
          , _pageMetadataTotalPages = computeTotalPage count sizeI
          , _pageMetadataNumber = pageI
          }
  return $ Page (T.unpack collection) metadata entities

createFindEntitiesByFn collection queryParams = do
  let action = rest =<< find (select queryParams collection)
  entitiesS <- runDB action
  liftEither . deserializeEntities $ entitiesS

createFindEntityFn collection entityName = do
  let action = findOne $ select [] collection
  maybeEntityS <- runDB action
  liftEither . deserializeMaybeEntity entityName "nothing" $ maybeEntityS

createFindEntityFn' collection entityName = do
  let action = findOne $ select [] collection
  maybeEntityS <- runDB action
  case deserializeMaybeEntity entityName "nothing" maybeEntityS of
    Right entity -> return (Just entity)
    Left (NotExistsError error) -> return Nothing
    Left error -> throwError error

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
  liftEither . Right $ count

createCountQueryFn collection query = do
  let action = count $ select query collection
  count <- runDB action
  liftEither . Right $ count

createFindFileFn bucketName fileName = do
  bucket <- runDB $ openBucket bucketName
  let action = fetchFile bucket ["filename" =: fileName]
  file <- runDB action
  result <- runDB (sourceFile file $$ CL.fold BS.append "")
  liftEither . Right $ result

createCreateFileFn bucketName fileName content = do
  bucket <- runDB $ openBucket bucketName
  runDB (runConduit $ yield content .| sinkFile bucket (T.pack fileName))
  return ()

createDeleteFilesFn bucketName = do
  bucket <- runDB $ openBucket bucketName
  let action = findFile bucket []
  files <- runDB action
  forM_ (fmap deleteFile files) runDB

createDeleteFilesByFn bucketName queryParams = do
  bucket <- runDB $ openBucket bucketName
  let action = findFile bucket queryParams
  files <- runDB action
  forM_ (fmap deleteFile files) runDB

createDeleteFileByFn bucketName paramValue = do
  bucket <- runDB $ openBucket bucketName
  let action = fetchFile bucket ["filename" =: paramValue]
  file <- runDB action
  runDB $ deleteFile file

createEnsureTextIndex collection indexes = do
  let i = ["createIndexes" =: "questionnaires", "indexes" =: fmap (textIndex collection) indexes]
  let action = runCommand i
  runDB action

textIndex collection field = ["key" =: [field =: "text"], "name" =: (T.append collection . T.append "_" $ field)]

regex mQuery = ["$regex" =: ".*" ++ fromMaybe "" mQuery ++ ".*", "$options" =: "si"]

mapSort :: [Sort] -> Document
mapSort = foldl go []
  where
    go acc (Sort name Ascending) = acc ++ [T.pack name =: 1]
    go acc (Sort name Descending) = acc ++ [T.pack name =: -1]

mapToDBQueryParams :: Functor f => f (String, String) -> f Field
mapToDBQueryParams = fmap go
  where
    go :: (String, String) -> Field
    go (p, v) = T.pack p =: v

computeTotalPage :: Int -> Int -> Int
computeTotalPage 0 0 = 0
computeTotalPage _ 0 = 1
computeTotalPage count size = ceiling $ fromIntegral count / fromIntegral size

instance Val LT.Text where
  val = String . LT.toStrict
  cast' (String x) = Just . LT.fromStrict $ x
  cast' (Sym (Symbol x)) = Just . LT.fromStrict $ x
  cast' _ = Nothing
