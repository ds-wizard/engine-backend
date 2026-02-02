module Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAudit where

import Shared.Audit.Service.Audit.AuditService
import Shared.Coordinate.Model.Coordinate.Coordinate
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

auditBundleExport :: Coordinate -> AppContextM ()
auditBundleExport = logAudit "document_template_bundle" "export" . show

auditBundlePullFromRegistry :: Coordinate -> AppContextM ()
auditBundlePullFromRegistry = logAudit "document_template_bundle" "pullFromRegistry" . show

auditBundleImportFromFile :: Coordinate -> AppContextM ()
auditBundleImportFromFile = logAudit "document_template_bundle" "importFromFile" . show
