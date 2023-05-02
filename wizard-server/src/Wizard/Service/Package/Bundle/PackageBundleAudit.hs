module Wizard.Service.Package.Bundle.PackageBundleAudit where

import Wizard.Model.Context.AppContext
import Wizard.Service.Audit.AuditService

auditPackageBundleExport :: String -> AppContextM ()
auditPackageBundleExport = logAudit "package_bundle" "export"

auditPackageBundlePullFromRegistry :: String -> AppContextM ()
auditPackageBundlePullFromRegistry = logAudit "package_bundle" "pullFromRegistry"

auditPackageBundleImportFromFile :: String -> AppContextM ()
auditPackageBundleImportFromFile = logAudit "package_bundle" "importFromFile"
