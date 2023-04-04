module Wizard.Service.Owl.OwlMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Util.Coordinate

fromOwl :: String -> String -> String -> String -> Maybe String -> [Event] -> U.UUID -> UTCTime -> PackageWithEvents
fromOwl name organizationId kmId version mPreviousPackageId events appUuid now =
  PackageWithEvents
    { pId = buildCoordinate organizationId kmId version
    , name = name
    , organizationId = organizationId
    , kmId = kmId
    , version = version
    , phase = ReleasedPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = ""
    , readme = ""
    , license = ""
    , previousPackageId = mPreviousPackageId
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , events = events
    , appUuid = appUuid
    , createdAt = now
    }
