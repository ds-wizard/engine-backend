module Database.BSON.Event.EventField where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Map

import Database.BSON.Common
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Model.Event.EventField
import Model.KnowledgeModel.KnowledgeModel

instance ToBSON (EventField String) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField String) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField (Maybe Int)) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField (Maybe Int)) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField (Maybe String)) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField (Maybe String)) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField [String]) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField [String]) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField (Maybe [String])) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField (Maybe [String])) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField (Map String String)) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField (Map String String)) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField QuestionValueType) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: serializeQuestionValueType value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField QuestionValueType) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- deserializeQuestionValueType $ BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField (Maybe QuestionValueType)) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: serializeMaybeQuestionValueType value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField (Maybe QuestionValueType)) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- deserializeMaybeQuestionValueType $ BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField [MetricMeasure]) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField [MetricMeasure]) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField [Tag]) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField [Tag]) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged
