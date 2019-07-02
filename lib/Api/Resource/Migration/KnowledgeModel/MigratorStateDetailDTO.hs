module Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO where

import qualified Data.UUID as U

import Api.Resource.Event.EventDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.Migration.KnowledgeModel.MigratorState

data MigratorStateDetailDTO = MigratorStateDetailDTO
  { _migratorStateDetailDTOBranchUuid :: U.UUID
  , _migratorStateDetailDTOMetamodelVersion :: Int
  , _migratorStateDetailDTOMigrationState :: MigrationState
  , _migratorStateDetailDTOBranchParentId :: String
  , _migratorStateDetailDTOTargetPackageId :: String
  , _migratorStateDetailDTOBranchEvents :: [EventDTO]
  , _migratorStateDetailDTOTargetPackageEvents :: [EventDTO]
  , _migratorStateDetailDTOResultEvents :: [EventDTO]
  , _migratorStateDetailDTOCurrentKnowledgeModel :: Maybe KnowledgeModelDTO
  } deriving (Show, Eq)
