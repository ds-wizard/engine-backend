module Shared.Database.Mapping.Package.PackageWithEventsRaw where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Package.PackageWithEventsRaw

instance ToRow PackageWithEventsRaw where
  toRow PackageWithEventsRaw {..} =
    [ toField _packageWithEventsRawPId
    , toField _packageWithEventsRawName
    , toField _packageWithEventsRawOrganizationId
    , toField _packageWithEventsRawKmId
    , toField _packageWithEventsRawVersion
    , toField _packageWithEventsRawMetamodelVersion
    , toField _packageWithEventsRawDescription
    , toField _packageWithEventsRawReadme
    , toField _packageWithEventsRawLicense
    , toField _packageWithEventsRawPreviousPackageId
    , toField _packageWithEventsRawForkOfPackageId
    , toField _packageWithEventsRawMergeCheckpointPackageId
    , toJSONField _packageWithEventsRawEvents
    , toField _packageWithEventsRawCreatedAt
    ]

instance FromRow PackageWithEventsRaw where
  fromRow = do
    _packageWithEventsRawPId <- field
    _packageWithEventsRawName <- field
    _packageWithEventsRawOrganizationId <- field
    _packageWithEventsRawKmId <- field
    _packageWithEventsRawVersion <- field
    _packageWithEventsRawMetamodelVersion <- field
    _packageWithEventsRawDescription <- field
    _packageWithEventsRawReadme <- field
    _packageWithEventsRawLicense <- field
    _packageWithEventsRawPreviousPackageId <- field
    _packageWithEventsRawForkOfPackageId <- field
    _packageWithEventsRawMergeCheckpointPackageId <- field
    _packageWithEventsRawEvents <- fieldWith fromJSONField
    _packageWithEventsRawCreatedAt <- field
    return $ PackageWithEventsRaw {..}
