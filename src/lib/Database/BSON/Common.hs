module Database.BSON.Common where

import Control.Lens
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.UUID

import Common.Error
import Common.Utils

serializeUUID :: UUID -> String
serializeUUID = toString

serializeMaybeUUID :: Maybe UUID -> Maybe String
serializeMaybeUUID mUuid = do
  uuid <- mUuid
  return . serializeUUID $ uuid

serializeUUIDList :: [UUID] -> [String]
serializeUUIDList = fmap toString

serializeMaybeUUIDList :: Maybe [UUID] -> Maybe [String]
serializeMaybeUUIDList mUuids = do
  uuids <- mUuids
  return $ serializeUUIDList uuids

deserializeUUID :: Maybe String -> Maybe UUID
deserializeUUID mUuidS = do
  uuidS <- mUuidS
  fromString uuidS

deserializeUUIDList :: [String] -> Maybe [UUID]
deserializeUUIDList uuidsS = switchMaybeAndList $ fmap fromString uuidsS

deserializeMaybeUUIDList :: Maybe [String] -> Maybe [UUID]
deserializeMaybeUUIDList mUuidsS = do
  uuidsS <- mUuidsS
  switchMaybeAndList $ fmap fromString uuidsS

instance ToBSON AppError where
  toBSON (ValidationError message formErrors fieldErrors) =
    [ "errorType" BSON.=: "ValidationError"
    , "message" BSON.=: message
    , "formErrors" BSON.=: formErrors
    , "fieldErrors" BSON.=: map (^.. each) fieldErrors
    ]
  toBSON (ForbiddenError message) = ["errorType" BSON.=: "ForbiddenError", "message" BSON.=: message]
  toBSON (NotExistsError message) = ["errorType" BSON.=: "NotExistsError", "message" BSON.=: message]
  toBSON (DatabaseError message) = ["errorType" BSON.=: "DatabaseError", "message" BSON.=: message]
  toBSON (MigratorError message) = ["errorType" BSON.=: "MigratorError", "message" BSON.=: message]

instance FromBSON AppError where
  fromBSON doc = do
    errorType <- BSON.lookup "errorType" doc
    case errorType of
      "ValidationError" -> do
        message <- BSON.lookup "message" doc
        formErrors <- BSON.lookup "formErrors" doc
        fieldErrors <- BSON.lookup "fieldErrors" doc
        return $ ValidationError message formErrors (tuplify2 <$> fieldErrors)
      "ForbiddenError" -> do
        message <- BSON.lookup "message" doc
        return $ ForbiddenError message
      "NotExistsError" -> do
        message <- BSON.lookup "message" doc
        return $ NotExistsError message
      "DatabaseError" -> do
        message <- BSON.lookup "message" doc
        return $ DatabaseError message
      "MigratorError" -> do
        message <- BSON.lookup "message" doc
        return $ MigratorError message
