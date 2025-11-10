module Wizard.Service.KnowledgeModel.Bundle.KnowledgeModelBundleAudit where

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditKnowledgeModelBundleExport :: String -> AppContextM ()
auditKnowledgeModelBundleExport = logAudit "knowledge_model_bundle" "export"

auditKnowledgeModelBundlePullFromRegistry :: String -> AppContextM ()
auditKnowledgeModelBundlePullFromRegistry = logAudit "knowledge_model_bundle" "pullFromRegistry"

auditKnowledgeModelBundleImportFromFile :: String -> AppContextM ()
auditKnowledgeModelBundleImportFromFile = logAudit "knowledge_model_bundle" "importFromFile"
