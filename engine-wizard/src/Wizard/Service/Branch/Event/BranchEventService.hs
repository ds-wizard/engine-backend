module Wizard.Service.Branch.Event.BranchEventService where

import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Common
import Wizard.Model.Branch.BranchData
import Wizard.Model.Context.AppContext
import Wizard.Service.Branch.Collaboration.CollaborationService
import Wizard.Service.KnowledgeModel.Squash.Squasher
import Wizard.Util.Logger

squashEvents :: AppContextM ()
squashEvents = do
  branchUuids <- findBranchesForSquashing
  traverse_ squashEventsForBranch branchUuids

squashEventsForBranch :: U.UUID -> AppContextM ()
squashEventsForBranch branchUuid =
  runInTransaction $ do
    logInfoU _CMP_SERVICE (f' "Squashing events for branch (branchUuid: '%s')" [U.toString branchUuid])
    logOutOnlineUsersWhenBranchDramaticallyChanged branchUuid
    branchData <- findBranchDataByIdForSquashingLocked branchUuid
    let squashedEvents = squash branchData.events
    updateBranchEventsByUuid branchUuid squashedEvents
    logInfoU
      _CMP_SERVICE
      ( f'
          "Squashing for branch '%s' finished successfully (before: %s, after %s)"
          [U.toString branchUuid, show . length $ branchData.events, show . length $ squashedEvents]
      )
