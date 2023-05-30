module Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAudit where

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditBundleExport :: String -> AppContextM ()
auditBundleExport = logAudit "template_bundle" "export"

auditBundlePullFromRegistry :: String -> AppContextM ()
auditBundlePullFromRegistry = logAudit "template_bundle" "pullFromRegistry"

auditBundleImportFromFile :: String -> AppContextM ()
auditBundleImportFromFile = logAudit "template_bundle" "importFromFile"
