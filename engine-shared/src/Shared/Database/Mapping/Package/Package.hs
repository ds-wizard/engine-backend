module Shared.Database.Mapping.Package.Package where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Database.Mapping.Package.PackagePhase ()
import Shared.Model.Package.Package

instance FromRow Package where
  fromRow = do
    pId <- field
    name <- field
    organizationId <- field
    kmId <- field
    version <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    previousPackageId <- field
    forkOfPackageId <- field
    mergeCheckpointPackageId <- field
    _ <- field :: RowParser Value
    createdAt <- field
    appUuid <- field
    phase <- field
    return $ Package {..}
