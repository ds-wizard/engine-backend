module Wizard.Service.Owl.OwlMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

fromOwl :: String -> String -> String -> String -> Maybe String -> U.UUID -> UTCTime -> KnowledgeModelPackage
fromOwl name organizationId kmId version mPreviousPackageId tenantUuid now =
  KnowledgeModelPackage
    { pId = buildCoordinate organizationId kmId version
    , name = name
    , organizationId = organizationId
    , kmId = kmId
    , version = version
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = ""
    , readme = ""
    , license = ""
    , previousPackageId = mPreviousPackageId
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , nonEditable = False
    , tenantUuid = tenantUuid
    , createdAt = now
    }
