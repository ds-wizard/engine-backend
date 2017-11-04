module Database.Migration.Package.PackageMigration where

import Data.Maybe
import qualified Data.UUID as U

import Context
import Database.DAO.Package.PackageDAO
import Service.Package.PackageService

runMigration context dspConfig = do
  putStrLn "MIGRATION (Package/Package): started"
  deletePackages context
  maybeBaseElixir0PackageDto <-
    createPackage context "Elixir Base" "elixir-base" "0.0.1" Nothing
  maybeBaseElixirPackageDto <-
    createPackage context "Elixir Base" "elixir-base" "1.0.0" Nothing
  createPackage
    context
    "Elixir Netherlands"
    "elixir-nl"
    "1.0.0"
    (Just maybeBaseElixirPackageDto)
  putStrLn "MIGRATION (Package/Package): ended"
