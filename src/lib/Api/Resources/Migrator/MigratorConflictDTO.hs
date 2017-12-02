module Api.Resources.Migrator.MigratorConflictDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Api.Resources.Event.EventDTO
import Api.Resources.KnowledgeModel.KnowledgeModelDTO
import Api.Resources.Migrator.Common
import Common.Types
import Common.Uuid
import Model.Migrator.MigratorState

data MigratorConflictDTO = MigratorConflictDTO
  { _mcdtoOriginalEventUuid :: UUID
  , _mcdtoAction :: MigrationConflictAction
  , _mcdtoEvent :: Maybe EventDTO
  } deriving (Show, Eq)

makeLenses ''MigratorConflictDTO

instance FromJSON MigratorConflictDTO where
  parseJSON (Object o) = do
    _mcdtoOriginalEventUuid <- o .: "originalEventUuid"
    action <- o .: "action"
    case getAction action of
      (Just _mcdtoAction) -> do
        _mcdtoEvent <- o .: "event"
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
      [ "originalEventUuid" .= _mcdtoOriginalEventUuid
      , "action" .=
        case _mcdtoAction of
          MCAApply -> "Apply"
          MCAEdited -> "Edited"
          MCAReject -> "Reject"
      , "event" .= _mcdtoEvent
      ]
