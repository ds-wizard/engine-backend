module Shared.Database.Mapping.Package.PackageWithEventsRaw where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Database.Mapping.Package.PackagePhase ()
import Shared.Model.Package.PackageWithEventsRaw

instance ToRow PackageWithEventsRaw where
  toRow PackageWithEventsRaw {..} =
    [ toField pId
    , toField name
    , toField organizationId
    , toField kmId
    , toField version
    , toField metamodelVersion
    , toField description
    , toField readme
    , toField license
    , toField previousPackageId
    , toField forkOfPackageId
    , toField mergeCheckpointPackageId
    , toJSONField events
    , toField createdAt
    , toField appUuid
    , toField phase
    ]

instance FromRow PackageWithEventsRaw where
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
    events <- fieldWith fromJSONField
    createdAt <- field
    appUuid <- field
    phase <- field
    return $ PackageWithEventsRaw {..}
