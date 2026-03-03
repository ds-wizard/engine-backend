module Wizard.Service.Owl.OwlMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

fromOwl :: U.UUID -> String -> String -> String -> String -> Maybe U.UUID -> U.UUID -> UTCTime -> KnowledgeModelPackage
fromOwl uuid name organizationId kmId version mPreviousPackageUuid tenantUuid now =
  KnowledgeModelPackage
    { uuid = uuid
    , name = name
    , organizationId = organizationId
    , kmId = kmId
    , version = version
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = knowledgeModelMetamodelVersion
    , description = ""
    , readme = ""
    , license = ""
    , previousPackageUuid = mPreviousPackageUuid
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , nonEditable = False
    , public = False
    , tenantUuid = tenantUuid
    , createdAt = now
    }
