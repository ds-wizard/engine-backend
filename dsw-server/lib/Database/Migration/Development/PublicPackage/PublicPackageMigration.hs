module Database.Migration.Development.PublicPackage.PublicPackageMigration where

import Constant.Component
import Database.DAO.PublicPackage.PublicPackageDAO
import Database.Migration.Development.PublicPackage.Data.PublicPackages
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(PublicPackage/PublicPackage) started"
  deletePublicPackages
  insertPublicPackage publicPackage
  logInfo $ msg _CMP_MIGRATION "(PublicPackage/PublicPackage) ended"
