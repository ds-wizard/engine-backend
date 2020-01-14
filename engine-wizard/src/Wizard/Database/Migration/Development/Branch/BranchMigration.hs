module Wizard.Database.Migration.Development.Branch.BranchMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Wizard.Constant.Component
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Event.EventDAO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.Branch.BranchService
import Wizard.Service.User.UserMapper
import Wizard.Util.Logger

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
