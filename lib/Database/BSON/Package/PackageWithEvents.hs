module Database.BSON.Package.PackageWithEvents where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Database.BSON.Event.Answer ()
import Database.BSON.Event.Chapter ()
import Database.BSON.Event.Common
import Database.BSON.Event.Expert ()
import Database.BSON.Event.KnowledgeModel ()
import Database.BSON.Event.Question ()
import Database.BSON.Event.Reference ()
import Model.Package.PackageWithEvents

instance ToBSON PackageWithEvents where
  toBSON PackageWithEvents {..} =
    [ "id" BSON.=: _packageWithEventsPId
    , "name" BSON.=: _packageWithEventsName
    , "organizationId" BSON.=: _packageWithEventsOrganizationId
    , "kmId" BSON.=: _packageWithEventsKmId
    , "version" BSON.=: _packageWithEventsVersion
    , "metamodelVersion" BSON.=: _packageWithEventsMetamodelVersion
    , "description" BSON.=: _packageWithEventsDescription
    , "readme" BSON.=: _packageWithEventsReadme
    , "license" BSON.=: _packageWithEventsLicense
    , "parentPackageId" BSON.=: _packageWithEventsParentPackageId
    , "events" BSON.=: convertEventToBSON <$> _packageWithEventsEvents
    , "createdAt" BSON.=: _packageWithEventsCreatedAt
    ]

instance FromBSON PackageWithEvents where
  fromBSON doc = do
    _packageWithEventsPId <- BSON.lookup "id" doc
    _packageWithEventsName <- BSON.lookup "name" doc
    _packageWithEventsOrganizationId <- BSON.lookup "organizationId" doc
    _packageWithEventsKmId <- BSON.lookup "kmId" doc
    _packageWithEventsVersion <- BSON.lookup "version" doc
    _packageWithEventsMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    _packageWithEventsDescription <- BSON.lookup "description" doc
    _packageWithEventsReadme <- BSON.lookup "readme" doc
    _packageWithEventsLicense <- BSON.lookup "license" doc
    _packageWithEventsParentPackageId <- BSON.lookup "parentPackageId" doc
    pkgEventsSerialized <- BSON.lookup "events" doc
    let _packageWithEventsEvents = (fromJust . chooseEventDeserializator) <$> pkgEventsSerialized
    _packageWithEventsCreatedAt <- BSON.lookup "createdAt" doc
    return PackageWithEvents {..}
