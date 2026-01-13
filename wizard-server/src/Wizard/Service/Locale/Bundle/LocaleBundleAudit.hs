module Wizard.Service.Locale.Bundle.LocaleBundleAudit where

import Shared.Audit.Service.Audit.AuditService
import Shared.Coordinate.Model.Coordinate.Coordinate
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditLocaleBundleExport :: Coordinate -> AppContextM ()
auditLocaleBundleExport = logAudit "locale_bundle" "export" . show

auditLocaleBundlePullFromRegistry :: Coordinate -> AppContextM ()
auditLocaleBundlePullFromRegistry = logAudit "locale_bundle" "pullFromRegistry" . show

auditLocaleBundleImportFromFile :: Coordinate -> AppContextM ()
auditLocaleBundleImportFromFile = logAudit "locale_bundle" "importFromFile" . show
