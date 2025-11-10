module Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorState where

import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField

import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState

instance FromField KnowledgeModelEditorState where
  fromField f dat =
    case fmap BS.unpack dat of
      Just "DefaultKnowledgeModelEditorState" -> return DefaultKnowledgeModelEditorState
      Just "EditedKnowledgeModelEditorState" -> return EditedKnowledgeModelEditorState
      Just "OutdatedKnowledgeModelEditorState" -> return OutdatedKnowledgeModelEditorState
      Just "MigratingKnowledgeModelEditorState" -> return MigratingKnowledgeModelEditorState
      Just "MigratedKnowledgeModelEditorState" -> return MigratedKnowledgeModelEditorState
      _ -> returnError ConversionFailed f "Unsupported type"
