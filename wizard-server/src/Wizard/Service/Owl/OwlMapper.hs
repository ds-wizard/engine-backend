module Wizard.Service.Owl.OwlMapper where

import Data.Time
import qualified Data.UUID as U

import WizardLib.Common.Util.Coordinate
import WizardLib.KnowledgeModel.Constant.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

fromOwl :: String -> String -> String -> String -> Maybe String -> [Event] -> U.UUID -> UTCTime -> PackageWithEvents
fromOwl name organizationId kmId version mPreviousPackageId events tenantUuid now =
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
    , nonEditable = False
    , tenantUuid = tenantUuid
    , createdAt = now
    }
