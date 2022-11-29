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

fromChangeDTO :: BranchChangeDTO -> U.UUID -> Maybe String -> Maybe U.UUID -> U.UUID -> UTCTime -> UTCTime -> Branch
fromChangeDTO dto bUuid bPackageId mCreatedBy appUuid bCreatedAt bUpdatedAt =
  Branch
    { uuid = bUuid
    , name = dto.name
    , kmId = dto.kmId
    , previousPackageId = bPackageId
    , createdBy = mCreatedBy
    , appUuid = appUuid
    , createdAt = bCreatedAt
    , updatedAt = bUpdatedAt
    }

fromCreateDTO :: BranchCreateDTO -> U.UUID -> Maybe U.UUID -> U.UUID -> UTCTime -> UTCTime -> Branch
fromCreateDTO dto bUuid mCreatedBy appUuid bCreatedAt bUpdatedAt =
  Branch
    { uuid = bUuid
    , name = dto.name
    , kmId = dto.kmId
    , previousPackageId = dto.previousPackageId
    , createdBy = mCreatedBy
    , appUuid = appUuid
    , createdAt = bCreatedAt
    , updatedAt = bUpdatedAt
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
