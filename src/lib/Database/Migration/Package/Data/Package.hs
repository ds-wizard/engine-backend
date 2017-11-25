module Database.Migration.Package.Data.Package where

import Control.Lens ((^.))

import Database.Migration.Branch.Data.Event.Event
import Model.Event.Event
import Model.Package.Package
import Service.Package.PackageMapper
import Service.Package.PackageService

baseElixir0PackageDto = buildPackage "Elixir Base Package" "elixir.base" "core" "0.0.1" "Beta version" Nothing []

baseElixirPackageDto =
  buildPackage
    "Elixir Base Package"
    "elixir.base"
    "core"
    "1.0.0"
    "First Release"
    Nothing
    [AddKnowledgeModelEvent' a_km1]

elixirNlPackageDto =
  buildPackage
    "Elixir Netherlands"
    "elixir.nl"
    "core-nl"
    "1.0.0"
    "First Release"
    (Just $ baseElixirPackageDto ^. pkgweId)
    [AddChapterEvent' a_km1_ch1]
