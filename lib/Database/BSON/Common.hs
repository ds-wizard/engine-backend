module Database.BSON.Common where

import Control.Lens
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Map (Map, fromList, toList)
import Data.UUID

import Model.Error.Error
import Model.Event.EventField
import Model.KnowledgeModel.KnowledgeModel
import Util.List

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

serializeEventFieldUUID :: EventField UUID -> EventField String
serializeEventFieldUUID efUuid = serializeUUID <$> efUuid

serializeEventFieldUUIDList :: EventField [UUID] -> EventField [String]
serializeEventFieldUUIDList efUuids = serializeUUIDList <$> efUuids

serializeEventFieldMaybeUUIDList :: EventField (Maybe [UUID]) -> EventField (Maybe [String])
serializeEventFieldMaybeUUIDList efMaybeUuids =
  case efMaybeUuids of
    ChangedValue maybeUuids -> ChangedValue $ serializeUUIDList <$> maybeUuids
    NothingChanged -> NothingChanged

serializeQuestionValueType :: QuestionValueType -> String
serializeQuestionValueType StringQuestionValueType = "StringValue"
serializeQuestionValueType NumberQuestionValueType = "NumberValue"
serializeQuestionValueType DateQuestionValueType = "DateValue"
serializeQuestionValueType TextQuestionValueType = "TextValue"

serializeMaybeQuestionValueType :: Maybe QuestionValueType -> Maybe String
serializeMaybeQuestionValueType mQuestionValueType = serializeQuestionValueType <$> mQuestionValueType

deserializeMaybeUUID :: Maybe String -> Maybe UUID
deserializeMaybeUUID mUuidS = do
  uuidS <- mUuidS
  fromString uuidS

deserializeUUIDList :: [String] -> Maybe [UUID]
deserializeUUIDList uuidsS = foldMaybe $ fmap fromString uuidsS

deserializeMaybeUUIDList :: Maybe [String] -> Maybe [UUID]
deserializeMaybeUUIDList mUuidsS = do
  uuidsS <- mUuidsS
  foldMaybe $ fmap fromString uuidsS

deserializeMaybeEventFieldUUID :: Maybe (EventField String) -> Maybe (EventField UUID)
deserializeMaybeEventFieldUUID maybeEfUuidS =
  case maybeEfUuidS of
    Just efUuidS ->
      case efUuidS of
        ChangedValue uuidS ->
          case fromString uuidS of
            Just uuid -> Just . ChangedValue $ uuid
            Nothing -> Nothing
        NothingChanged -> Just NothingChanged
    Nothing -> Nothing

deserializeEventFieldUUIDList :: Maybe (EventField [String]) -> EventField [UUID]
deserializeEventFieldUUIDList maybeEfUuids =
  case maybeEfUuids of
    Just efUuids -> extractMaybe $ deserializeUUIDList <$> efUuids
    Nothing -> NothingChanged
  where
    extractMaybe :: EventField (Maybe [UUID]) -> EventField [UUID]
    extractMaybe maybeUuids =
      case maybeUuids of
        (ChangedValue (Just uuids)) -> ChangedValue uuids
        (ChangedValue Nothing) -> NothingChanged
        NothingChanged -> NothingChanged

deserializeEventFieldMaybeUUIDList :: Maybe (EventField (Maybe [String])) -> EventField (Maybe [UUID])
deserializeEventFieldMaybeUUIDList maybeEfMaybeUuidsS =
  case maybeEfMaybeUuidsS of
    Just efMaybeUuidsS ->
      case efMaybeUuidsS of
        ChangedValue maybeUuidsS ->
          case deserializeUUIDList <$> maybeUuidsS of
            Just maybeUuids -> ChangedValue maybeUuids
            Nothing -> NothingChanged
        NothingChanged -> NothingChanged
    Nothing -> NothingChanged

deserializeQuestionValueType :: Maybe String -> Maybe QuestionValueType
deserializeQuestionValueType mQuestionValueTypeS = do
  questionValueTypeS <- mQuestionValueTypeS
  case questionValueTypeS of
    "StringValue" -> Just StringQuestionValueType
    "NumberValue" -> Just NumberQuestionValueType
    "DateValue" -> Just DateQuestionValueType
    "TextValue" -> Just TextQuestionValueType
    _ -> Nothing

deserializeMaybeQuestionValueType :: Maybe String -> Maybe (Maybe QuestionValueType)
deserializeMaybeQuestionValueType mQuestionValueTypeS = do
  questionValueType <- mQuestionValueTypeS
  case questionValueType of
    "StringValue" -> Just . Just $ StringQuestionValueType
    "NumberValue" -> Just . Just $ NumberQuestionValueType
    "DateValue" -> Just . Just $ DateQuestionValueType
    "TextValue" -> Just . Just $ TextQuestionValueType
    _ -> Just Nothing

deserializeEventFieldQuestionValueType :: Maybe String -> EventField QuestionValueType
deserializeEventFieldQuestionValueType mQuestionValueTypeS =
  case mQuestionValueTypeS of
    Just questionValueType ->
      case questionValueType of
        "StringValue" -> ChangedValue StringQuestionValueType
        "NumberValue" -> ChangedValue NumberQuestionValueType
        "DateValue" -> ChangedValue DateQuestionValueType
        "TextValue" -> ChangedValue TextQuestionValueType
        _ -> NothingChanged
    Nothing -> NothingChanged

instance ToBSON AppError where
  toBSON (ValidationError message formErrors fieldErrors) =
    [ "errorType" BSON.=: "ValidationError"
    , "message" BSON.=: message
    , "formErrors" BSON.=: formErrors
    , "fieldErrors" BSON.=: map (^.. each) fieldErrors
    ]
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
      "NotExistsError" -> do
        message <- BSON.lookup "message" doc
        return $ NotExistsError message
      "DatabaseError" -> do
        message <- BSON.lookup "message" doc
        return $ DatabaseError message
      "MigratorError" -> do
        message <- BSON.lookup "message" doc
        return $ MigratorError message

instance ToBSON (String, String) where
  toBSON (key, value) = ["key" BSON.=: key, "value" BSON.=: value]

instance FromBSON (String, String) where
  fromBSON doc = do
    key <- BSON.lookup "key" doc
    value <- BSON.lookup "value" doc
    return (key, value)

instance ToBSON (Map String String) where
  toBSON m = ["map" BSON.=: toList m]

instance FromBSON (Map String String) where
  fromBSON doc = do
    mList <- BSON.lookup "map" doc
    return $ fromList mList
