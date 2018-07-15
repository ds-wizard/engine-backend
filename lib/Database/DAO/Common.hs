module Database.DAO.Common where

import Control.Monad.Reader (asks, liftIO)
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import qualified Data.Text.Lazy as LT
import qualified Data.UUID as U
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Localization
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

deserializeMaybeEntity :: (FromBSON a) => Maybe Document -> Either AppError a
deserializeMaybeEntity mEntityS =
  case mEntityS of
    Just entityS -> do
      let mEntity = fromBSON entityS
      case mEntity of
        Just entity -> Right entity
        Nothing -> Left . DatabaseError $ _ERROR_DATABASE__DESERIALIZATION_FAILED
    Nothing -> Left . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND

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
