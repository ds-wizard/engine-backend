module Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAudit where

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditBundleExport :: String -> AppContextM ()
auditBundleExport = logAudit "document_template_bundle" "export"

auditBundlePullFromRegistry :: String -> AppContextM ()
auditBundlePullFromRegistry = logAudit "document_template_bundle" "pullFromRegistry"

auditBundleImportFromFile :: String -> AppContextM ()
auditBundleImportFromFile = logAudit "document_template_bundle" "importFromFile"
