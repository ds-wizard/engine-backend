module Wizard.Database.Migration.Development.PublicPackage.PublicPackageMigration where

import Wizard.Constant.Component
import Wizard.Database.DAO.PublicPackage.PublicPackageDAO
import Wizard.Database.Migration.Development.PublicPackage.Data.PublicPackages
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(PublicPackage/PublicPackage) started"
  deletePublicPackages
  insertPublicPackage publicPackage
  logInfo _CMP_MIGRATION "(PublicPackage/PublicPackage) ended"
