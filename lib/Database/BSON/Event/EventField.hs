module Database.BSON.Event.EventField where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Typeable
import GHC.Generics

import Database.BSON.Common
import Database.BSON.KnowledgeModel.KnowledgeModel
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

instance ToBSON (EventField (Maybe AnswerItemTemplate)) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField (Maybe AnswerItemTemplate)) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField (Maybe AnswerItemTemplatePlain)) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField (Maybe AnswerItemTemplatePlain)) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField (Maybe AnswerItemTemplatePlainWithIds)) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField (Maybe AnswerItemTemplatePlainWithIds)) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField QuestionType) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: serializeQuestionType value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField QuestionType) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- deserializeQuestionType $ BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged

instance ToBSON (EventField (Maybe QuestionType)) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: serializeMaybeQuestionType value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance FromBSON (EventField (Maybe QuestionType)) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- deserializeMaybeQuestionType $ BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged
