module Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Event.EventDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Migration.KnowledgeModel.MigrationStateDTO

data MigratorStateDetailDTO = MigratorStateDetailDTO
  { _migratorStateDetailDTOBranchUuid :: U.UUID
  , _migratorStateDetailDTOMetamodelVersion :: Int
  , _migratorStateDetailDTOMigrationState :: MigrationStateDTO
  , _migratorStateDetailDTOBranchPreviousPackageId :: String
  , _migratorStateDetailDTOTargetPackageId :: String
  , _migratorStateDetailDTOBranchEvents :: [EventDTO]
  , _migratorStateDetailDTOTargetPackageEvents :: [EventDTO]
  , _migratorStateDetailDTOResultEvents :: [EventDTO]
  , _migratorStateDetailDTOCurrentKnowledgeModel :: Maybe KnowledgeModelDTO
  } deriving (Show, Eq, Generic)
