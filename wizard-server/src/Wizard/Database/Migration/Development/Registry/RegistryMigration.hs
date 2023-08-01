module Wizard.Database.Migration.Development.Registry.RegistryMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryPackageDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryPackages
import Wizard.Database.Migration.Development.Registry.Data.RegistryTemplates
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Registry/Package) started"
  deleteRegistryOrganizations
  deleteRegistryPackages
  deleteRegistryTemplates
  insertRegistryOrganization globalRegistryOrganization
  insertRegistryOrganization nlRegistryOrganization
  insertRegistryPackage globalRegistryPackage
  insertRegistryPackage nlRegistryPackage
  insertRegistryTemplate commonWizardRegistryTemplate
  logInfo _CMP_MIGRATION "(Registry/Package) ended"
