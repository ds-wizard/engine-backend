module Api.Resource.Migration.KnowledgeModel.MigratorConflictJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Migration.KnowledgeModel.Common ()
import Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Model.Migration.KnowledgeModel.MigratorState

instance FromJSON MigratorConflictDTO where
  parseJSON (Object o) = do
    _migratorConflictDTOOriginalEventUuid <- o .: "originalEventUuid"
    action <- o .: "action"
    case getAction action of
      (Just _migratorConflictDTOAction) -> do
        _migratorConflictDTOEvent <- o .: "event"
        return MigratorConflictDTO {..}
      Nothing -> fail "Unsupported conflict action"
    where
      getAction "Apply" = Just MCAApply
      getAction "Edited" = Just MCAEdited
      getAction "Reject" = Just MCAReject
      getAction _ = Nothing
  parseJSON _ = mzero

instance ToJSON MigratorConflictDTO where
  toJSON MigratorConflictDTO {..} =
    object
      [ "originalEventUuid" .= _migratorConflictDTOOriginalEventUuid
      , "action" .=
        case _migratorConflictDTOAction of
          MCAApply -> "Apply"
          MCAEdited -> "Edited"
          MCAReject -> "Reject"
      , "event" .= _migratorConflictDTOEvent
      ]
