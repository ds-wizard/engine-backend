module Database.Migration.Package.PackageMigration where

import Data.Maybe
import qualified Data.UUID as U

import Common.Context
import Database.DAO.Package.PackageDAO
import Database.Migration.KnowledgeModel.Data.Event.Event
import Model.Event.Event
import Service.Package.PackageMapper
import Service.Package.PackageService

runMigration context dspConfig logState = do
  logState "MIGRATION (Package/Package): started"
  deletePackages context
  let baseElixir0PackageDto =
        buildPackage
          "Elixir Base Package"
          "elixir.base"
          "core"
          "0.0.1"
          "Beta version"
          Nothing
          []
  insertPackage context baseElixir0PackageDto
  let baseElixirPackageDto =
        buildPackage
          "Elixir Base Package"
          "elixir.base"
          "core"
          "1.0.0"
          "First Release"
          Nothing
          [AddKnowledgeModelEvent' a_km1]
  insertPackage context baseElixirPackageDto
  let elixirNlPackageDto =
        buildPackage
          "Elixir Netherlands"
          "elixir.nl"
          "core-nl"
          "1.0.0"
          "First Release"
          (Just baseElixirPackageDto)
          [AddChapterEvent' a_km1_ch1]
  insertPackage context elixirNlPackageDto
  logState "MIGRATION (Package/Package): ended"
