module Wizard.Service.Owl.OwlMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Coordinate

fromOwl :: String -> String -> String -> String -> Maybe String -> [Event] -> U.UUID -> UTCTime -> PackageWithEvents
fromOwl name organizationId kmId version mPreviousPackageId events appUuid now =
  PackageWithEvents
    { _packageWithEventsPId = buildCoordinate organizationId kmId version
    , _packageWithEventsName = name
    , _packageWithEventsOrganizationId = organizationId
    , _packageWithEventsKmId = kmId
    , _packageWithEventsVersion = version
    , _packageWithEventsMetamodelVersion = kmMetamodelVersion
    , _packageWithEventsDescription = ""
    , _packageWithEventsReadme = ""
    , _packageWithEventsLicense = ""
    , _packageWithEventsPreviousPackageId = mPreviousPackageId
    , _packageWithEventsForkOfPackageId = Nothing
    , _packageWithEventsMergeCheckpointPackageId = Nothing
    , _packageWithEventsEvents = events
    , _packageWithEventsAppUuid = appUuid
    , _packageWithEventsCreatedAt = now
    }
