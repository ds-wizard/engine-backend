module Wizard.Service.Branch.BranchAudit where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditBranchPublish :: Branch -> BranchData -> Maybe String -> AppContextM ()
auditBranchPublish branch branchData mForkOfPkgId =
  logAuditWithBody
    "branch"
    "publish"
    (U.toString $ branch.uuid)
    ( M.fromList
        [ ("kmId", branch.kmId)
        , ("eventSize", show . length $ branchData.events)
        , ("isFork", show . isJust $ mForkOfPkgId)
        ]
    )
