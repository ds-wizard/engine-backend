module Wizard.Service.Package.PackageAudit where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Model.Package.Package
import Wizard.Model.Branch.Branch
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Audit.AuditService

auditPackageFailedToDelete :: String -> String -> String -> AppContextM ()
auditPackageFailedToDelete entityId reasonType reasonId =
  logAuditWithBody "package" "failedToDelete" entityId (M.fromList [("reasonType", reasonType), ("reasonId", reasonId)])

auditPackageFailedToDeleteDuePreviousPackages :: String -> [Package] -> AppContextM ()
auditPackageFailedToDeleteDuePreviousPackages entityId pkgs =
  auditPackageFailedToDelete entityId "PreviousPackage" (show $ fmap (.pId) pkgs)

auditPackageFailedToDeleteDueParentPackages :: String -> [Package] -> AppContextM ()
auditPackageFailedToDeleteDueParentPackages entityId pkgs =
  auditPackageFailedToDelete entityId "ParentPackage" (show $ fmap (.pId) pkgs)

auditPackageFailedToDeleteDueBranches :: String -> [Branch] -> AppContextM ()
auditPackageFailedToDeleteDueBranches entityId branches =
  auditPackageFailedToDelete entityId "Branch" (show $ fmap (\b -> U.toString $ b.uuid) branches)

auditPackageFailedToDeleteDueQuestionnaires :: String -> [Questionnaire] -> AppContextM ()
auditPackageFailedToDeleteDueQuestionnaires entityId questionnaires =
  auditPackageFailedToDelete entityId "Branch" (show $ fmap (\qtn -> U.toString $ qtn.uuid) questionnaires)
