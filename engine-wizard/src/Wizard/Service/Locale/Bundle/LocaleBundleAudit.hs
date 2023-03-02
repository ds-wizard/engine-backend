module Wizard.Service.Locale.Bundle.LocaleBundleAudit where

import Wizard.Model.Context.AppContext
import Wizard.Service.Audit.AuditService

auditLocaleBundleExport :: String -> AppContextM ()
auditLocaleBundleExport = logAudit "locale_bundle" "export"

auditLocaleBundlePullFromRegistry :: String -> AppContextM ()
auditLocaleBundlePullFromRegistry = logAudit "locale_bundle" "pullFromRegistry"

auditLocaleBundleImportFromFile :: String -> AppContextM ()
auditLocaleBundleImportFromFile = logAudit "locale_bundle" "importFromFile"
