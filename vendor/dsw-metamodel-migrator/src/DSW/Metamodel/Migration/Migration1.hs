module DSW.Metamodel.Migration.Migration1 where

import Data.Aeson
import Data.Either

import qualified DSW.Metamodel.Event.Version1 as V1
import qualified DSW.Metamodel.Event.Version2 as V2

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

class (ToJSON f, FromJSON t) => Upgradeable f t where
  upgrade :: f -> Either String t

instance Upgradeable V1.EventPathDTO V2.EventPathDTO where
  upgrade = result2Either . fromJSON . toJSON

instance (FromJSON a, ToJSON a) => Upgradeable (V1.EventFieldDTO a) (V2.EventFieldDTO a) where
  upgrade V1.NothingChangedDTO = Right V2.NothingChangedDTO
  upgrade (V1.ChangedValueDTO x) = Right (V2.ChangedValueDTO x)

instance Upgradeable V1.EditKnowledgeModelEventDTO V2.EditKnowledgeModelEventDTO where
  upgrade (V1.EditKnowledgeModelEventDTO {..}) = do
    newPath <- upgrade _editKnowledgeModelEventDTOPath
    newName <- upgrade _editKnowledgeModelEventDTOName
    newChapterUuids <- upgrade _editKnowledgeModelEventDTOChapterUuids
    newTagUuids <- upgrade _editKnowledgeModelEventDTOTagUuids
    return
      V2.EditKnowledgeModelEventDTO
      { V2._editKnowledgeModelEventDTOUuid = _editKnowledgeModelEventDTOUuid
      , V2._editKnowledgeModelEventDTOPath = newPath
      , V2._editKnowledgeModelEventDTOKmUuid = _editKnowledgeModelEventDTOKmUuid
      , V2._editKnowledgeModelEventDTOName = newName
      , V2._editKnowledgeModelEventDTOChapterUuids = newChapterUuids
      , V2._editKnowledgeModelEventDTOTagUuids = newTagUuids
      , V2._editKnowledgeModelEventDTOIntegrationUuids = V2.NothingChangedDTO
      }

instance Upgradeable V1.EventDTO V2.EventDTO where
  upgrade (V1.EditKnowledgeModelEventDTO' oldEvent) = do
    newEvent <- upgrade oldEvent
    return $ V2.EditKnowledgeModelEventDTO' newEvent
  upgrade x = result2Either . fromJSON . toJSON $ x

migrateEventValue :: Value -> Either String Value
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V1.EventDTO)
  return $ toJSON (newEvent :: V2.EventDTO)
