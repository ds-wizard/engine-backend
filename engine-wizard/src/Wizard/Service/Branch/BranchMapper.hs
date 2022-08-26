module Wizard.Service.Branch.BranchMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
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
    { _branchListUuid = branch ^. uuid
    , _branchListName = branch ^. name
    , _branchListKmId = branch ^. kmId
    , _branchListState = state
    , _branchListPreviousPackageId = branch ^. previousPackageId
    , _branchListForkOfPackageId = mForkOfPackageId
    , _branchListOwnerUuid = branch ^. ownerUuid
    , _branchListCreatedAt = branch ^. createdAt
    , _branchListUpdatedAt = branch ^. updatedAt
    }

toDetailDTO :: Branch -> BranchData -> KnowledgeModel -> Maybe String -> Maybe Package -> BranchState -> BranchDetailDTO
toDetailDTO branch branchData knowledgeModel mForkOfPackageId mForkOfPackage state =
  BranchDetailDTO
    { _branchDetailDTOUuid = branch ^. uuid
    , _branchDetailDTOName = branch ^. name
    , _branchDetailDTOKmId = branch ^. kmId
    , _branchDetailDTOState = state
    , _branchDetailDTOPreviousPackageId = branch ^. previousPackageId
    , _branchDetailDTOForkOfPackageId = mForkOfPackageId
    , _branchDetailDTOForkOfPackage = fmap toSimpleDTO mForkOfPackage
    , _branchDetailDTOEvents = branchData ^. events
    , _branchDetailDTOKnowledgeModel = knowledgeModel
    , _branchDetailDTOOwnerUuid = branch ^. ownerUuid
    , _branchDetailDTOCreatedAt = branch ^. createdAt
    , _branchDetailDTOUpdatedAt = branch ^. updatedAt
    }

fromChangeDTO :: BranchChangeDTO -> U.UUID -> Maybe String -> Maybe U.UUID -> U.UUID -> UTCTime -> UTCTime -> Branch
fromChangeDTO dto bUuid bPackageId mOwnerUuid appUuid bCreatedAt bUpdatedAt =
  Branch
    { _branchUuid = bUuid
    , _branchName = dto ^. name
    , _branchKmId = dto ^. kmId
    , _branchPreviousPackageId = bPackageId
    , _branchOwnerUuid = mOwnerUuid
    , _branchAppUuid = appUuid
    , _branchCreatedAt = bCreatedAt
    , _branchUpdatedAt = bUpdatedAt
    }

fromCreateDTO :: BranchCreateDTO -> U.UUID -> Maybe U.UUID -> U.UUID -> UTCTime -> UTCTime -> Branch
fromCreateDTO dto bUuid mOwnerUuid appUuid bCreatedAt bUpdatedAt =
  Branch
    { _branchUuid = bUuid
    , _branchName = dto ^. name
    , _branchKmId = dto ^. kmId
    , _branchPreviousPackageId = dto ^. previousPackageId
    , _branchOwnerUuid = mOwnerUuid
    , _branchAppUuid = appUuid
    , _branchCreatedAt = bCreatedAt
    , _branchUpdatedAt = bUpdatedAt
    }

toBranchData :: Branch -> BranchData
toBranchData branch =
  BranchData
    { _branchDataBranchUuid = branch ^. uuid
    , _branchDataMetamodelVersion = kmMetamodelVersion
    , _branchDataEvents = []
    , _branchDataAppUuid = branch ^. appUuid
    , _branchDataCreatedAt = branch ^. createdAt
    , _branchDataUpdatedAt = branch ^. updatedAt
    }
