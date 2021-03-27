module Shared.Database.Mapping.Package.PackageWithEvents where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.Event.EventJM ()
import Shared.Model.Package.PackageWithEvents

instance ToRow PackageWithEvents where
  toRow PackageWithEvents {..} =
    [ toField _packageWithEventsPId
    , toField _packageWithEventsName
    , toField _packageWithEventsOrganizationId
    , toField _packageWithEventsKmId
    , toField _packageWithEventsVersion
    , toField _packageWithEventsMetamodelVersion
    , toField _packageWithEventsDescription
    , toField _packageWithEventsReadme
    , toField _packageWithEventsLicense
    , toField _packageWithEventsPreviousPackageId
    , toField _packageWithEventsForkOfPackageId
    , toField _packageWithEventsMergeCheckpointPackageId
    , toJSONField _packageWithEventsEvents
    , toField _packageWithEventsCreatedAt
    ]

instance FromRow PackageWithEvents where
  fromRow = do
    _packageWithEventsPId <- field
    _packageWithEventsName <- field
    _packageWithEventsOrganizationId <- field
    _packageWithEventsKmId <- field
    _packageWithEventsVersion <- field
    _packageWithEventsMetamodelVersion <- field
    _packageWithEventsDescription <- field
    _packageWithEventsReadme <- field
    _packageWithEventsLicense <- field
    _packageWithEventsPreviousPackageId <- field
    _packageWithEventsForkOfPackageId <- field
    _packageWithEventsMergeCheckpointPackageId <- field
    _packageWithEventsEvents <- fieldWith fromJSONField
    _packageWithEventsCreatedAt <- field
    return $ PackageWithEvents {..}
