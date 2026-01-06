module Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageAudit where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.Project.Project

auditPackageFailedToDelete :: String -> String -> String -> AppContextM ()
auditPackageFailedToDelete entityId reasonType reasonId =
  logAuditWithBody "package" "failedToDelete" entityId (M.fromList [("reasonType", reasonType), ("reasonId", reasonId)])

auditPackageFailedToDeleteDuePreviousPackages :: String -> [KnowledgeModelPackage] -> AppContextM ()
auditPackageFailedToDeleteDuePreviousPackages entityId pkgs =
  auditPackageFailedToDelete entityId "PreviousPackage" (show $ fmap (.pId) pkgs)

auditPackageFailedToDeleteDueParentPackages :: String -> [KnowledgeModelPackage] -> AppContextM ()
auditPackageFailedToDeleteDueParentPackages entityId pkgs =
  auditPackageFailedToDelete entityId "ParentPackage" (show $ fmap (.pId) pkgs)

auditPackageFailedToDeleteDueKmEditors :: String -> [KnowledgeModelEditor] -> AppContextM ()
auditPackageFailedToDeleteDueKmEditors entityId knowledgeModelEditors =
  auditPackageFailedToDelete entityId "Knowledge Model Editor" (show $ fmap (\b -> U.toString $ b.uuid) knowledgeModelEditors)

auditPackageFailedToDeleteDueProjects :: String -> [Project] -> AppContextM ()
auditPackageFailedToDeleteDueProjects entityId projects =
  auditPackageFailedToDelete entityId "Knowledge Model Editor" (show $ fmap (\project -> U.toString $ project.uuid) projects)
