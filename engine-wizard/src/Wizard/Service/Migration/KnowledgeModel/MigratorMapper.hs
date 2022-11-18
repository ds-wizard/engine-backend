module Wizard.Service.Migration.KnowledgeModel.MigratorMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Migration.KnowledgeModel.MigratorState

toDTO :: MigratorState -> Branch -> MigratorStateDTO
toDTO ms branch =
  MigratorStateDTO
    { branchUuid = ms.branchUuid
    , branchName = branch.name
    , branchPreviousPackageId = ms.branchPreviousPackageId
    , migrationState = ms.migrationState
    , targetPackageId = ms.targetPackageId
    , currentKnowledgeModel = ms.currentKnowledgeModel
    }

fromCreateDTO
  :: Branch -> Package -> [Event] -> String -> [Event] -> KnowledgeModel -> U.UUID -> UTCTime -> MigratorState
fromCreateDTO branch previousPkg branchEvents targetPkgId targetPkgEvents km appUuid now =
  MigratorState
    { branchUuid = branch.uuid
    , metamodelVersion = kmMetamodelVersion
    , migrationState = RunningState
    , branchPreviousPackageId = previousPkg.pId
    , targetPackageId = targetPkgId
    , branchEvents = branchEvents
    , targetPackageEvents = targetPkgEvents
    , resultEvents = []
    , currentKnowledgeModel = Just km
    , appUuid = appUuid
    , createdAt = now
    }
