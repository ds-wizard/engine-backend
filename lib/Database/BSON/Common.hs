module Database.BSON.Common where

import Control.Lens
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe (fromMaybe)
import Data.UUID

import Common.Error
import Common.Utils
import Model.Event.EventField
import Model.KnowledgeModel.KnowledgeModel

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

serializeEventFieldUUIDList :: EventField [UUID] -> EventField [String]
serializeEventFieldUUIDList efUuids = serializeUUIDList <$> efUuids

serializeEventFieldMaybeUUIDList :: EventField (Maybe [UUID]) -> EventField (Maybe [String])
serializeEventFieldMaybeUUIDList efMaybeUuids =
  case efMaybeUuids of
    ChangedValue maybeUuids -> ChangedValue $ serializeUUIDList <$> maybeUuids
    NothingChanged -> NothingChanged

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

deserializeQuestionType :: Maybe String -> Maybe QuestionType
deserializeQuestionType mQuestionTypeS = do
  questionType <- mQuestionTypeS
  case questionType of
    "QuestionTypeOption" -> Just QuestionTypeOption
    "QuestionTypeList" -> Just QuestionTypeList
    "QuestionString" -> Just QuestionString
    "QuestionNumber" -> Just QuestionNumber
    "QuestionDate" -> Just QuestionDate
    "QuestionText" -> Just QuestionText
    _ -> Nothing

deserializeEventFieldQuestionType :: Maybe String -> EventField QuestionType
deserializeEventFieldQuestionType mQuestionTypeS =
  case mQuestionTypeS of
    Just questionType ->
      case questionType of
        "QuestionTypeOption" -> ChangedValue QuestionTypeOption
        "QuestionTypeList" -> ChangedValue QuestionTypeList
        "QuestionString" -> ChangedValue QuestionString
        "QuestionNumber" -> ChangedValue QuestionNumber
        "QuestionDate" -> ChangedValue QuestionDate
        "QuestionText" -> ChangedValue QuestionText
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
