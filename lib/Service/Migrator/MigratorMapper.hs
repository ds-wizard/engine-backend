module Service.Migrator.MigratorMapper where

import Control.Lens ((^.))

import Api.Resource.Migrator.MigratorStateDTO
import Model.Migrator.MigratorState
import Service.KnowledgeModel.KnowledgeModelMapper

toDTO :: MigratorState -> MigratorStateDTO
toDTO ms =
  MigratorStateDTO
  { _msdtoBranchUuid = ms ^. msBranchUuid
  , _msdtoMigrationState = ms ^. msMigrationState
  , _msdtoBranchParentId = ms ^. msBranchParentId
  , _msdtoTargetPackageId = ms ^. msTargetPackageId
  , _msdtoCurrentKnowledgeModel = toKnowledgeModelDTO <$> ms ^. msCurrentKnowledgeModel
  }
