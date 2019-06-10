module Database.BSON.Common where

import Control.Lens
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Map (Map, fromList, toList)
import qualified Data.Text as T
import Data.UUID

import Model.ActionKey.ActionKey
import Model.Error.Error
import Model.Event.EventField
import Model.KnowledgeModel.KnowledgeModel
import Model.Questionnaire.Questionnaire
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

serializeActionType :: ActionKeyType -> String
serializeActionType RegistrationActionKey = "RegistrationActionKey"
serializeActionType ForgottenPasswordActionKey = "ForgottenPasswordActionKey"

serializeQuestionValueType :: QuestionValueType -> String
serializeQuestionValueType StringQuestionValueType = "StringQuestionValueType"
serializeQuestionValueType NumberQuestionValueType = "NumberQuestionValueType"
serializeQuestionValueType DateQuestionValueType = "DateQuestionValueType"
serializeQuestionValueType TextQuestionValueType = "TextQuestionValueType"

serializeQuestionnaireAccessibility :: QuestionnaireAccessibility -> String
serializeQuestionnaireAccessibility PublicQuestionnaire = "PublicQuestionnaire"
serializeQuestionnaireAccessibility PrivateQuestionnaire = "PrivateQuestionnaire"
serializeQuestionnaireAccessibility PublicReadOnlyQuestionnaire = "PublicReadOnlyQuestionnaire"

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

deserializeActionType :: Maybe String -> Maybe ActionKeyType
deserializeActionType mActionTypeS = do
  actionType <- mActionTypeS
  case actionType of
    "RegistrationActionKey" -> Just RegistrationActionKey
    "ForgottenPasswordActionKey" -> Just ForgottenPasswordActionKey
    _ -> Nothing

deserializeQuestionnaireAccessibility :: Maybe String -> Maybe QuestionnaireAccessibility
deserializeQuestionnaireAccessibility mAccessibilityS = do
  accessibility <- mAccessibilityS
  case accessibility of
    "PublicQuestionnaire" -> Just PublicQuestionnaire
    "PrivateQuestionnaire" -> Just PrivateQuestionnaire
    "PublicReadOnlyQuestionnaire" -> Just PublicReadOnlyQuestionnaire
    _ -> Nothing

deserializeQuestionValueType :: Maybe String -> Maybe QuestionValueType
deserializeQuestionValueType mQuestionValueTypeS = do
  questionValueTypeS <- mQuestionValueTypeS
  case questionValueTypeS of
    "StringQuestionValueType" -> Just StringQuestionValueType
    "NumberQuestionValueType" -> Just NumberQuestionValueType
    "DateQuestionValueType" -> Just DateQuestionValueType
    "TextQuestionValueType" -> Just TextQuestionValueType
    _ -> Nothing

deserializeMaybeQuestionValueType :: Maybe String -> Maybe (Maybe QuestionValueType)
deserializeMaybeQuestionValueType mQuestionValueTypeS = do
  questionValueType <- mQuestionValueTypeS
  case questionValueType of
    "StringQuestionValueType" -> Just . Just $ StringQuestionValueType
    "NumberQuestionValueType" -> Just . Just $ NumberQuestionValueType
    "DateQuestionValueType" -> Just . Just $ DateQuestionValueType
    "TextQuestionValueType" -> Just . Just $ TextQuestionValueType
    _ -> Just Nothing

deserializeEventFieldQuestionValueType :: Maybe String -> EventField QuestionValueType
deserializeEventFieldQuestionValueType mQuestionValueTypeS =
  case mQuestionValueTypeS of
    Just questionValueType ->
      case questionValueType of
        "StringQuestionValueType" -> ChangedValue StringQuestionValueType
        "NumberQuestionValueType" -> ChangedValue NumberQuestionValueType
        "DateQuestionValueType" -> ChangedValue DateQuestionValueType
        "TextQuestionValueType" -> ChangedValue TextQuestionValueType
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
  toBSON m = fmap (\(k, v) -> (T.pack k) BSON.=: v) (toList m)

instance FromBSON (Map String String) where
  fromBSON doc = Just . fromList . fmap (\f -> (T.unpack . BSON.label $ f, BSON.typed . BSON.value $ f)) $ doc
