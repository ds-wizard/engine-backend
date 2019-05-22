module Api.Resource.Package.Common where

import Data.Aeson

import Model.Package.PackageState

serializePackageState :: PackageState -> String
serializePackageState UnknownPackageState = "UnknownPackageState"
serializePackageState OutdatedPackageState = "OutdatedPackageState"
serializePackageState UpToDatePackageState = "UpToDatePackageState"
serializePackageState UnpublishedPackageState = "UnpublishedPackageState"

deserializePackageState :: String -> Maybe PackageState
deserializePackageState "UnknownPackageState" = Just UnknownPackageState
deserializePackageState "OutdatedPackageState" = Just OutdatedPackageState
deserializePackageState "UpToDatePackageState" = Just UpToDatePackageState
deserializePackageState "UnpublishedPackageState" = Just UnpublishedPackageState
deserializePackageState _ = Nothing

hDeserializePackageState o callback = do
  fieldS <- o .: "state"
  case deserializePackageState fieldS of
    (Just field) -> callback field
    Nothing -> fail "Unsupported package state"
