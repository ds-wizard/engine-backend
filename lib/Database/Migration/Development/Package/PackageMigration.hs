module Database.Migration.Development.Package.PackageMigration where

import Control.Monad.Logger (logInfo)

import Database.DAO.Package.PackageDAO
import Database.Migration.Development.Package.Data.Packages

runMigration = do
  $(logInfo) "MIGRATION (Package/Package): started"
  deletePackages
  insertPackage baseElixir0PackageDto
  insertPackage baseElixirPackageDto
  insertPackage elixirNlPackageDto
  insertPackage elixirNlPackage2Dto
  $(logInfo) "MIGRATION (Package/Package): ended"
