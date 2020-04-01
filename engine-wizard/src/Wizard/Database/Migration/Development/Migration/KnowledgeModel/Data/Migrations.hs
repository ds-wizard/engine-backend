module Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Service.Event.EventMapper
import Shared.Service.KnowledgeModel.KnowledgeModelMapper
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Model.Migration.KnowledgeModel.MigratorState

migratorState :: MigratorStateDTO
migratorState =
  MigratorStateDTO
    { _migratorStateDTOBranchUuid = amsterdamBranch ^. uuid
    , _migratorStateDTOMigrationState =
        ConflictStateDTO . CorrectorConflictDTO . toDTOFn . Prelude.head $ netherlandsPackageV2 ^. events
    , _migratorStateDTOBranchPreviousPackageId = netherlandsPackage ^. pId
    , _migratorStateDTOTargetPackageId = netherlandsPackageV2 ^. pId
    , _migratorStateDTOCurrentKnowledgeModel = Just . toKnowledgeModelDTO $ km1Netherlands
    }

migratorStateCreate :: MigratorStateCreateDTO
migratorStateCreate = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}

migratorConflict :: MigratorConflictDTO
migratorConflict =
  MigratorConflictDTO
    { _migratorConflictDTOOriginalEventUuid = a_km1_ch4 ^. uuid
    , _migratorConflictDTOAction = MCAEdited
    , _migratorConflictDTOEvent = Just . toDTOFn . Prelude.head $ netherlandsPackageV2 ^. events
    }
