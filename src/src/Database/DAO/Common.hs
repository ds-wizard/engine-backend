module Database.DAO.Common where

import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import qualified Data.Text.Lazy as LT
import qualified Data.UUID as U

import Common.Error

deserializeEntities
  :: (FromBSON a)
  => [Document] -> Either AppError [a]
deserializeEntities entitiesS = do
  let maybeEntities = fromBSON <$> entitiesS
  let entities = catMaybes maybeEntities
  if length maybeEntities == length entities
    then Right entities
    else Left . DatabaseError $
         "Problem with deserialization of entity from database"

deserializeMaybeEntity
  :: (FromBSON a)
  => Maybe Document -> Either AppError a
deserializeMaybeEntity mEntityS =
  case mEntityS of
    Just entityS -> do
      let mEntity = fromBSON entityS
      case mEntity of
        Just entity -> Right entity
        Nothing ->
          Left . DatabaseError $
          "Problem with deserialization of entity from database"
    Nothing -> Left . NotExistsError $ "Entity is not exists"

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
