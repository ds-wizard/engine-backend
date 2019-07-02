module Api.Resource.Migration.KnowledgeModel.MigratorStateDetailJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Event.EventJM ()
import Api.Resource.KnowledgeModel.KnowledgeModelDTO ()
import Api.Resource.Migration.KnowledgeModel.Common ()
import Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO

instance FromJSON MigratorStateDetailDTO where
  parseJSON (Object o) = do
    _migratorStateDetailDTOBranchUuid <- o .: "branchUuid"
    _migratorStateDetailDTOMetamodelVersion <- o .: "metamodelVersion"
    _migratorStateDetailDTOMigrationState <- o .: "migrationState"
    _migratorStateDetailDTOBranchParentId <- o .: "branchParentId"
    _migratorStateDetailDTOTargetPackageId <- o .: "targetPackageId"
    _migratorStateDetailDTOBranchEvents <- o .: "branchEvents"
    _migratorStateDetailDTOTargetPackageEvents <- o .: "targetPackageEvents"
    _migratorStateDetailDTOResultEvents <- o .: "resultEvents"
    _migratorStateDetailDTOCurrentKnowledgeModel <- o .: "currentKnowledgeModel"
    return MigratorStateDetailDTO {..}
  parseJSON _ = mzero

instance ToJSON MigratorStateDetailDTO where
  toJSON MigratorStateDetailDTO {..} =
    object
      [ "branchUuid" .= _migratorStateDetailDTOBranchUuid
      , "metamodelVersion" .= _migratorStateDetailDTOMetamodelVersion
      , "migrationState" .= _migratorStateDetailDTOMigrationState
      , "branchParentId" .= _migratorStateDetailDTOBranchParentId
      , "targetPackageId" .= _migratorStateDetailDTOTargetPackageId
      , "branchEvents" .= _migratorStateDetailDTOBranchEvents
      , "targetPackageEvents" .= _migratorStateDetailDTOTargetPackageEvents
      , "resultEvents" .= _migratorStateDetailDTOResultEvents
      , "currentKnowledgeModel" .= _migratorStateDetailDTOCurrentKnowledgeModel
      ]
