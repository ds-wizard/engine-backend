module Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations where

import Shared.Common.Util.Date
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchList
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Model.Tenant.Tenant
import WizardLib.KnowledgeModel.Constant.KnowledgeModel
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

migratorState :: MigratorStateDTO
migratorState =
  MigratorStateDTO
    { branchUuid = amsterdamBranchList.uuid
    , branchName = amsterdamBranchList.name
    , branchPreviousPackageId = netherlandsPackage.pId
    , migrationState =
        ConflictState . CorrectorConflict . Just . Prelude.head $ netherlandsPackageV2.events
    , targetPackageId = netherlandsPackageV2.pId
    , currentKnowledgeModel = Just km1Netherlands
    }

migratorStateCreate :: MigratorStateCreateDTO
migratorStateCreate = MigratorStateCreateDTO {targetPackageId = netherlandsPackageV2.pId}

migratorConflict :: MigratorConflictDTO
migratorConflict =
  MigratorConflictDTO
    { originalEventUuid = a_km1_ch4.uuid
    , action = MCAEdited
    , event = Just . Prelude.head $ netherlandsPackageV2.events
    }

differentMigratorState :: MigratorState
differentMigratorState =
  MigratorState
    { branchUuid = differentBranch.uuid
    , metamodelVersion = kmMetamodelVersion
    , migrationState = CompletedState
    , branchPreviousPackageId = differentPackage.pId
    , targetPackageId = differentPackage.pId
    , branchEvents = []
    , targetPackageEvents = []
    , resultEvents = []
    , currentKnowledgeModel = Nothing
    , tenantUuid = differentTenant.uuid
    , createdAt = dt'' 2018 1 1 1
    }
