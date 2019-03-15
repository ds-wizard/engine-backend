module Database.Migration.Development.Branch.BranchMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import Constant.Component
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.Migration.Development.Branch.Data.Branches
import Database.Migration.Development.User.Data.Users
import LensesConfig
import Service.Branch.BranchService
import Service.User.UserMapper
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(KnowledgeModel/Branch) started"
  deleteBranches
  createBranchWithParams
    (amsterdamBranch ^. uuid)
    (amsterdamBranch ^. createdAt)
    (toDTO userAlbert)
    amsterdamBranchCreate
  updateEventsInBranch (U.toString $ amsterdamBranch ^. uuid) (amsterdamBranchWithEvents ^. events)
  logInfo $ msg _CMP_MIGRATION "(KnowledgeModel/Branch) ended"
