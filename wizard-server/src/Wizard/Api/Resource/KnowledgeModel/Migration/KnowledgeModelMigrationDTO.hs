module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

data KnowledgeModelMigrationDTO = KnowledgeModelMigrationDTO
  { editorUuid :: U.UUID
  , editorName :: String
  , editorPreviousPackage :: KnowledgeModelPackageSuggestion
  , state :: KnowledgeModelMigrationState
  , targetPackage :: KnowledgeModelPackageSuggestion
  , currentKnowledgeModel :: Maybe KnowledgeModel
  }
  deriving (Show, Eq, Generic)
