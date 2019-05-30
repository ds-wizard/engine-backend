module DSW.Metamodel.Migration.Migration2 where

import Data.Aeson
import Data.Either

import qualified DSW.Metamodel.Event.Version2 as V2
import qualified DSW.Metamodel.Event.Version3 as V3

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

class Upgradeable f t where
  upgrade :: f -> Either String t

instance Upgradeable V2.EventPathDTO V3.EventPathDTO where
  upgrade = result2Either . fromJSON . toJSON

instance (FromJSON a, ToJSON a) => Upgradeable (V2.EventFieldDTO a) (V3.EventFieldDTO a) where
  upgrade V2.NothingChangedDTO = Right V3.NothingChangedDTO
  upgrade (V2.ChangedValueDTO x) = Right (V3.ChangedValueDTO x)

instance Upgradeable V2.QuestionValueType V3.QuestionValueType where
  upgrade V2.StringQuestionValueType = Right V3.StringQuestionValueType
  upgrade V2.NumberQuestionValueType = Right V3.NumberQuestionValueType
  upgrade V2.DateQuestionValueType = Right V3.DateQuestionValueType
  upgrade V2.TextQuestionValueType = Right V3.TextQuestionValueType

instance Upgradeable (V2.EventFieldDTO V2.QuestionValueType) (V3.EventFieldDTO V3.QuestionValueType) where
  upgrade V2.NothingChangedDTO = Right V3.NothingChangedDTO
  upgrade (V2.ChangedValueDTO oldQuestionValueType) = do
    newQuestionValueType <- upgrade oldQuestionValueType
    return $ V3.ChangedValueDTO newQuestionValueType

instance Upgradeable V2.AddValueQuestionEventDTO V3.AddValueQuestionEventDTO where
  upgrade (V2.AddValueQuestionEventDTO {..}) = do
    newPath <- upgrade _addValueQuestionEventDTOPath
    newQuestionValueType <- upgrade _addValueQuestionEventDTOValueType
    return
      V3.AddValueQuestionEventDTO
      { V3._addValueQuestionEventDTOUuid = _addValueQuestionEventDTOUuid
      , V3._addValueQuestionEventDTOPath = newPath
      , V3._addValueQuestionEventDTOQuestionUuid = _addValueQuestionEventDTOQuestionUuid
      , V3._addValueQuestionEventDTOTitle = _addValueQuestionEventDTOTitle
      , V3._addValueQuestionEventDTOText = _addValueQuestionEventDTOText
      , V3._addValueQuestionEventDTORequiredLevel = _addValueQuestionEventDTORequiredLevel
      , V3._addValueQuestionEventDTOTagUuids = _addValueQuestionEventDTOTagUuids
      , V3._addValueQuestionEventDTOValueType = newQuestionValueType
      }

instance Upgradeable V2.EditValueQuestionEventDTO V3.EditValueQuestionEventDTO where
  upgrade (V2.EditValueQuestionEventDTO {..}) = do
    newPath <- upgrade _editValueQuestionEventDTOPath
    newTitle <- upgrade _editValueQuestionEventDTOTitle
    newText <- upgrade _editValueQuestionEventDTOText
    newRequiredLevel <- upgrade _editValueQuestionEventDTORequiredLevel
    newTagUuids <- upgrade _editValueQuestionEventDTOTagUuids
    newExpertUuids <- upgrade _editValueQuestionEventDTOExpertUuids
    newReferenceUuids <- upgrade _editValueQuestionEventDTOReferenceUuids
    newQuestionValueType <- upgrade _editValueQuestionEventDTOValueType
    return
      V3.EditValueQuestionEventDTO
      { V3._editValueQuestionEventDTOUuid = _editValueQuestionEventDTOUuid
      , V3._editValueQuestionEventDTOPath = newPath
      , V3._editValueQuestionEventDTOQuestionUuid = _editValueQuestionEventDTOQuestionUuid
      , V3._editValueQuestionEventDTOTitle = newTitle
      , V3._editValueQuestionEventDTOText = newText
      , V3._editValueQuestionEventDTORequiredLevel = newRequiredLevel
      , V3._editValueQuestionEventDTOTagUuids = newTagUuids
      , V3._editValueQuestionEventDTOExpertUuids = newExpertUuids
      , V3._editValueQuestionEventDTOReferenceUuids = newReferenceUuids
      , V3._editValueQuestionEventDTOValueType = newQuestionValueType
      }

instance Upgradeable V2.AddQuestionEventDTO V3.AddQuestionEventDTO where
  upgrade (V2.AddValueQuestionEventDTO' oldEvent) = do
    newEvent <- upgrade oldEvent
    return $ V3.AddValueQuestionEventDTO' newEvent
  upgrade x = result2Either . fromJSON . toJSON $ x

instance Upgradeable V2.EditQuestionEventDTO V3.EditQuestionEventDTO where
  upgrade (V2.EditValueQuestionEventDTO' oldEvent) = do
    newEvent <- upgrade oldEvent
    return $ V3.EditValueQuestionEventDTO' newEvent
  upgrade x = result2Either . fromJSON . toJSON $ x

instance Upgradeable V2.EventDTO V3.EventDTO where
  upgrade (V2.AddQuestionEventDTO' oldEvent) = do
    newEvent <- upgrade oldEvent
    return $ V3.AddQuestionEventDTO' newEvent
  upgrade (V2.EditQuestionEventDTO' oldEvent) = do
    newEvent <- upgrade oldEvent
    return $ V3.EditQuestionEventDTO' newEvent
  upgrade x = result2Either . fromJSON . toJSON $ x

migrateEventValue :: Value -> Either String Value
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V2.EventDTO)
  return $ toJSON (newEvent :: V3.EventDTO)
