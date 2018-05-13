module Database.Migration.Package.Data.Package where

import Control.Lens ((^.))

import Database.Migration.Branch.Data.Event.Event
import LensesConfig
import Model.Event.Event
import Service.Package.PackageMapper

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
    (Just $ baseElixirPackageDto ^. pId)
    [AddChapterEvent' a_km1_ch1]

elixirNlPackage2Dto =
  buildPackage
    "Elixir Netherlands"
    "elixir.nl"
    "core-nl"
    "2.0.0"
    "Second Release"
    (Just $ elixirNlPackageDto ^. pId)
    [AddChapterEvent' a_km1_ch3]
