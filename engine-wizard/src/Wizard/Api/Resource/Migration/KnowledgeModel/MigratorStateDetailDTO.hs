module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO

data MigratorStateDetailDTO =
  MigratorStateDetailDTO
    { _migratorStateDetailDTOBranchUuid :: U.UUID
    , _migratorStateDetailDTOMetamodelVersion :: Int
    , _migratorStateDetailDTOMigrationState :: MigrationStateDTO
    , _migratorStateDetailDTOBranchPreviousPackageId :: String
    , _migratorStateDetailDTOTargetPackageId :: String
    , _migratorStateDetailDTOBranchEvents :: [Event]
    , _migratorStateDetailDTOTargetPackageEvents :: [Event]
    , _migratorStateDetailDTOResultEvents :: [Event]
    , _migratorStateDetailDTOCurrentKnowledgeModel :: Maybe KnowledgeModel
    }
  deriving (Show, Eq, Generic)
