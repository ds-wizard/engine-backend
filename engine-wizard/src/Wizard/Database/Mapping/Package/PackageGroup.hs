module Wizard.Database.Mapping.Package.PackageGroup where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Model.Package.PackageGroup

instance FromRow PackageGroup where
  fromRow = do
    _packageGroupOrganizationId <- field
    _packageGroupKmId <- field
    _packageGroupVersions <- field
    return $ PackageGroup {..}
