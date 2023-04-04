module Wizard.Service.Branch.BranchMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Constant.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Branch.BranchList
import Wizard.Model.Branch.BranchState
import Wizard.Service.Package.PackageMapper

toList :: Branch -> Maybe String -> BranchState -> BranchList
toList branch mForkOfPackageId state =
  BranchList
    { uuid = branch.uuid
    , name = branch.name
    , kmId = branch.kmId
    , version = branch.version
    , state = state
    , previousPackageId = branch.previousPackageId
    , forkOfPackageId = mForkOfPackageId
    , createdBy = branch.createdBy
    , createdAt = branch.createdAt
    , updatedAt = branch.updatedAt
    }

toDetailDTO :: Branch -> BranchData -> KnowledgeModel -> Maybe String -> Maybe Package -> BranchState -> BranchDetailDTO
toDetailDTO branch branchData knowledgeModel mForkOfPackageId mForkOfPackage state =
  BranchDetailDTO
    { uuid = branch.uuid
    , name = branch.name
    , kmId = branch.kmId
    , version = branch.version
    , description = branch.description
    , readme = branch.readme
    , license = branch.license
    , state = state
    , previousPackageId = branch.previousPackageId
    , forkOfPackageId = mForkOfPackageId
    , forkOfPackage = fmap toSimpleDTO mForkOfPackage
    , events = branchData.events
    , knowledgeModel = knowledgeModel
    , createdBy = branch.createdBy
    , createdAt = branch.createdAt
    , updatedAt = branch.updatedAt
    }

toBranchData :: Branch -> BranchData
toBranchData branch =
  BranchData
    { branchUuid = branch.uuid
    , metamodelVersion = kmMetamodelVersion
    , events = []
    , appUuid = branch.appUuid
    , createdAt = branch.createdAt
    , updatedAt = branch.updatedAt
    }

fromCreateDTO :: BranchCreateDTO -> U.UUID -> Maybe Package -> U.UUID -> U.UUID -> UTCTime -> Branch
fromCreateDTO dto uuid mPreviousPkg createdBy appUuid now =
  Branch
    { uuid = uuid
    , name = dto.name
    , kmId = dto.kmId
    , version = dto.version
    , description = maybe "" (.description) mPreviousPkg
    , readme = maybe "" (.readme) mPreviousPkg
    , license = maybe "" (.license) mPreviousPkg
    , previousPackageId = dto.previousPackageId
    , createdBy = Just createdBy
    , appUuid = appUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: BranchChangeDTO -> Branch -> UTCTime -> Branch
fromChangeDTO dto branch bUpdatedAt =
  Branch
    { uuid = branch.uuid
    , name = dto.name
    , kmId = dto.kmId
    , version = dto.version
    , description = dto.description
    , readme = dto.readme
    , license = dto.license
    , previousPackageId = branch.previousPackageId
    , createdBy = branch.createdBy
    , appUuid = branch.appUuid
    , createdAt = branch.createdAt
    , updatedAt = bUpdatedAt
    }
