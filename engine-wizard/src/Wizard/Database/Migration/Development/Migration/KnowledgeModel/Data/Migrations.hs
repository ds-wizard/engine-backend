module Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations where

import Control.Lens ((^.))

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Model.Migration.KnowledgeModel.MigratorState

migratorState :: MigratorStateDTO
migratorState =
  MigratorStateDTO
    { _migratorStateDTOBranchUuid = amsterdamBranch ^. uuid
    , _migratorStateDTOBranchName = amsterdamBranch ^. name
    , _migratorStateDTOBranchPreviousPackageId = netherlandsPackage ^. pId
    , _migratorStateDTOMigrationState =
        ConflictState . CorrectorConflict . Just . Prelude.head $ netherlandsPackageV2 ^. events
    , _migratorStateDTOTargetPackageId = netherlandsPackageV2 ^. pId
    , _migratorStateDTOCurrentKnowledgeModel = Just km1Netherlands
    }

migratorStateCreate :: MigratorStateCreateDTO
migratorStateCreate = MigratorStateCreateDTO {_migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId}

migratorConflict :: MigratorConflictDTO
migratorConflict =
  MigratorConflictDTO
    { _migratorConflictDTOOriginalEventUuid = a_km1_ch4 ^. uuid
    , _migratorConflictDTOAction = MCAEdited
    , _migratorConflictDTOEvent = Just . Prelude.head $ netherlandsPackageV2 ^. events
    }

differentMigratorState :: MigratorState
differentMigratorState =
  MigratorState
    { _migratorStateBranchUuid = differentBranch ^. uuid
    , _migratorStateMetamodelVersion = kmMetamodelVersion
    , _migratorStateMigrationState = CompletedState
    , _migratorStateBranchPreviousPackageId = differentPackage ^. pId
    , _migratorStateTargetPackageId = differentPackage ^. pId
    , _migratorStateBranchEvents = []
    , _migratorStateTargetPackageEvents = []
    , _migratorStateResultEvents = []
    , _migratorStateCurrentKnowledgeModel = Nothing
    , _migratorStateAppUuid = differentApp ^. uuid
    }
