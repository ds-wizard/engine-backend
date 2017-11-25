module Service.Migrator.Migrator where

import Control.Lens ((^.), (&), (.~), makeLenses)
import Data.Either
import Data.Maybe
import qualified Data.UUID as U

import Common.Context
import Common.Error
import Database.DAO.Branch.BranchDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import Database.DAO.Package.PackageDAO
import Model.Branch.Branch
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Migrator.MigratorState
import Model.Package.Package
import Service.Branch.BranchService
import Service.Migrator.Applicator
import Service.Migrator.Methods.CleanerMethod
import Service.Migrator.Methods.CorrectorMethod
import Service.Package.PackageService

createMigration :: Context -> String -> String -> IO (Either AppError MigratorState)
createMigration context branchId targetPackageId =
  getBranch branchId $ \branch ->
    getTargetParentPackage targetPackageId $ \targetParentPackage ->
      getBranchEvents branch $ \branchEvents ->
        getTargetParentEvents targetPackageId branch $ \targetPackageEvents ->
          getParentPackageId branch $ \branchId -> do
            let km = branch ^. bwkmKM
            return . Right $ createMigratorStateWithEvents targetPackageId branchId branchEvents targetPackageEvents km
  where
    getBranch branchId callback = do
      eitherBranch <- findKnowledgeModelByBranchId context branchId
      case eitherBranch of
        Right branch -> callback branch
        Left (NotExistsError _) -> return . Left . MigratorError $ "Source branch does not exist"
        Left error -> return . Left $ error
    getTargetParentPackage targetPackageId callback = do
      eitherTargetParentPackage <- findPackageWithEventsById context targetPackageId
      case eitherTargetParentPackage of
        Right targetParentPackage -> callback targetParentPackage
        Left (NotExistsError _) -> return . Left . MigratorError $ "Target parent package does not exist"
        Left error -> return . Left $ error
    getBranchEvents branch callback =
      getParentPackageId branch $ \parentPackageId ->
        getLastMergeCheckpointPackageId branch $ \lastMergeCheckpointPackageId -> do
          let since = parentPackageId
          let until = lastMergeCheckpointPackageId
          eitherEvents <- getAllPreviousEventsSincePackageIdAndUntilPackageId context since until
          case eitherEvents of
            Right events -> callback events
            Left error -> return . Left $ error
    getParentPackageId branch callback =
      case branch ^. bwkmParentPackageId of
        Just parentPackageId -> callback parentPackageId
        Nothing -> return . Left . MigratorError $ "Branch has to have a parent"
    getLastMergeCheckpointPackageId branch callback =
      case branch ^. bwkmLastMergeCheckpointPackageId of
        Just lastMergeCheckpointPackageId -> callback lastMergeCheckpointPackageId
        Nothing -> return . Left . MigratorError $ "Branch has to have merge checkpoint"
    getTargetParentEvents targetPackageId branch callback =
      getLastAppliedParentPackageId branch $ \lastAppliedParentPackageId -> do
        let since = targetPackageId
        let until = lastAppliedParentPackageId
        eitherEvents <- getAllPreviousEventsSincePackageIdAndUntilPackageId context since until
        case eitherEvents of
          Right events -> callback events
          Left error -> return . Left $ error
    getLastAppliedParentPackageId branch callback =
      case branch ^. bwkmLastAppliedParentPackageId of
        Just lastAppliedParentPackageId -> callback lastAppliedParentPackageId
        Nothing ->
          return . Left . MigratorError $
          "Branch has to have checkpoint what was last parent package which was merged in"

createMigratorStateWithEvents :: String -> String -> [Event] -> [Event] -> Maybe KnowledgeModel -> MigratorState
createMigratorStateWithEvents branchParentId targetPackageId branchEvents targetPackageEvents mKm =
  MigratorState
  { _msMigrationState = RunningState
  , _msBranchParentId = branchParentId
  , _msTargetPackageId = targetPackageId
  , _msBranchEvents = branchEvents
  , _msTargetPackageEvents = targetPackageEvents
  , _msCurrentKnowledgeModel = mKm
  }

doMigrate :: MigratorState -> Event -> MigratorState
doMigrate state event =
  if isCleanerMethod state event
    then runCleanerMethod state event
    else runCorrectorMethod state event

migrate :: MigratorState -> MigratorState
migrate state =
  case state ^. msMigrationState of
    RunningState ->
      let newState = foldl doMigrate state (state ^. msTargetPackageEvents)
      in if newState ^. msTargetPackageEvents == []
           then newState & msMigrationState .~ CompletedState
           else newState
    ConflictState _ -> state
    ErrorState _ -> state
    CompletedState -> state
