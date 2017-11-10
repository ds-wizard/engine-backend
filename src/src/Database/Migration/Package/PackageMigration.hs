module Database.Migration.Package.PackageMigration where

import Data.Maybe
import qualified Data.UUID as U

import Context
import Database.DAO.Package.PackageDAO
import Database.Migration.KnowledgeModel.Data.Event.Event
import Model.Event.Event
import Service.Package.PackageMapper
import Service.Package.PackageService

runMigration context dspConfig = do
  putStrLn "MIGRATION (Package/Package): started"
  deletePackages context
  let baseElixir0PackageDto =
        buildPackage
          "Elixir Base"
          "elixir-base"
          "0.0.1"
          "Beta version"
          Nothing
          []
  insertPackage context baseElixir0PackageDto
  let baseElixirPackageDto =
        buildPackage
          "Elixir Base"
          "elixir-base"
          "1.0.0"
          "First Release"
          Nothing
          [AddKnowledgeModelEvent' a_km1]
  insertPackage context baseElixirPackageDto
  let elixirNlPackageDto =
        buildPackage
          "Elixir Netherlands"
          "elixir-nl"
          "1.0.0"
          "First Release"
          (Just baseElixirPackageDto)
          [AddChapterEvent' a_km1_ch1]
  insertPackage context elixirNlPackageDto
  putStrLn "MIGRATION (Package/Package): ended"
