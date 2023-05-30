module Wizard.Service.Package.Bundle.PackageBundleAudit where

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditPackageBundleExport :: String -> AppContextM ()
auditPackageBundleExport = logAudit "package_bundle" "export"

auditPackageBundlePullFromRegistry :: String -> AppContextM ()
auditPackageBundlePullFromRegistry = logAudit "package_bundle" "pullFromRegistry"

auditPackageBundleImportFromFile :: String -> AppContextM ()
auditPackageBundleImportFromFile = logAudit "package_bundle" "importFromFile"
