module Wizard.Database.Migration.Development.Branch.BranchMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Branch.BranchData
import Wizard.Model.Branch.BranchList
import Wizard.Service.Branch.BranchService
import Wizard.Service.User.UserMapper

runMigration = do
  logInfo _CMP_MIGRATION "(KnowledgeModel/Branch) started"
  deleteBranches
  createBranchWithParams
    amsterdamBranchList.uuid
    amsterdamBranchList.createdAt
    (toDTO userAlbert)
    amsterdamBranchCreate
  appendBranchEventByUuid amsterdamBranchList.uuid amsterdamBranchData.events
  insertBranch differentBranch
  logInfo _CMP_MIGRATION "(KnowledgeModel/Branch) ended"
