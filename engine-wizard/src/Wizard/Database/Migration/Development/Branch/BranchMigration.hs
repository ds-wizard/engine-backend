module Wizard.Database.Migration.Development.Branch.BranchMigration where

import Control.Lens ((^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.Component
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.Branch.BranchService
import Wizard.Service.User.UserMapper
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(KnowledgeModel/Branch) started"
  deleteBranches
  createBranchWithParams
    (amsterdamBranchDto ^. uuid)
    (amsterdamBranchDto ^. createdAt)
    (toDTO userAlbert)
    amsterdamBranchCreate
  appendBranchEventByUuid (U.toString $ amsterdamBranchDto ^. uuid) (amsterdamBranchData ^. events)
  insertBranch differentBranch
  logInfo _CMP_MIGRATION "(KnowledgeModel/Branch) ended"
