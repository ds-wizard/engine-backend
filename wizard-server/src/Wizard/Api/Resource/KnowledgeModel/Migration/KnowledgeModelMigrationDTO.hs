module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration

data KnowledgeModelMigrationDTO = KnowledgeModelMigrationDTO
  { editorUuid :: U.UUID
  , editorName :: String
  , editorPreviousPackageId :: String
  , state :: KnowledgeModelMigrationState
  , targetPackageId :: String
  , currentKnowledgeModel :: Maybe KnowledgeModel
  }
  deriving (Show, Eq, Generic)
