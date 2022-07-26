module Wizard.Service.TemplateBundle.TemplateBundleAudit where

import Wizard.Model.Context.AppContext
import Wizard.Service.Audit.AuditService

auditTemplateBundleExport :: String -> AppContextM ()
auditTemplateBundleExport = logAudit "template_bundle" "export"

auditTemplateBundlePullFromRegistry :: String -> AppContextM ()
auditTemplateBundlePullFromRegistry = logAudit "template_bundle" "pullFromRegistry"

auditTemplateBundleImportFromFile :: String -> AppContextM ()
auditTemplateBundleImportFromFile = logAudit "template_bundle" "importFromFile"
