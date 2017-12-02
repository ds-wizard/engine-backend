module Database.Migration.Package.PackageMigration where

import Data.Maybe
import qualified Data.UUID as U

import Common.Context
import Database.DAO.Package.PackageDAO
import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Package.Data.Package
import Model.Event.Event
import Service.Package.PackageMapper
import Service.Package.PackageService

runMigration context dspConfig logState = do
  logState "MIGRATION (Package/Package): started"
  deletePackages context
  insertPackage context baseElixir0PackageDto
  insertPackage context baseElixirPackageDto
  insertPackage context elixirNlPackageDto
  insertPackage context elixirNlPackage2Dto
  logState "MIGRATION (Package/Package): ended"
