module Wizard.Service.Branch.BranchMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Service.Event.EventMapper
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchWithEventsDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchState

toDTO :: BranchWithEvents -> Maybe String -> BranchState -> BranchDTO
toDTO branch mForkOfPackageId state =
  BranchDTO
    { _branchDTOUuid = branch ^. uuid
    , _branchDTOName = branch ^. name
    , _branchDTOKmId = branch ^. kmId
    , _branchDTOState = state
    , _branchDTOPreviousPackageId = branch ^. previousPackageId
    , _branchDTOForkOfPackageId = mForkOfPackageId
    , _branchDTOOwnerUuid = branch ^. ownerUuid
    , _branchDTOCreatedAt = branch ^. createdAt
    , _branchDTOUpdatedAt = branch ^. updatedAt
    }

toDetailDTO :: BranchWithEvents -> Maybe String -> BranchState -> BranchDetailDTO
toDetailDTO branch mForkOfPackageId state =
  BranchDetailDTO
    { _branchDetailDTOUuid = branch ^. uuid
    , _branchDetailDTOName = branch ^. name
    , _branchDetailDTOKmId = branch ^. kmId
    , _branchDetailDTOState = state
    , _branchDetailDTOPreviousPackageId = branch ^. previousPackageId
    , _branchDetailDTOForkOfPackageId = mForkOfPackageId
    , _branchDetailDTOEvents = toDTOs $ branch ^. events
    , _branchDetailDTOOwnerUuid = branch ^. ownerUuid
    , _branchDetailDTOCreatedAt = branch ^. createdAt
    , _branchDetailDTOUpdatedAt = branch ^. updatedAt
    }

fromWithEventsDTO :: BranchWithEventsDTO -> BranchWithEvents
fromWithEventsDTO dto =
  BranchWithEvents
    { _branchWithEventsUuid = dto ^. uuid
    , _branchWithEventsName = dto ^. name
    , _branchWithEventsKmId = dto ^. kmId
    , _branchWithEventsMetamodelVersion = dto ^. metamodelVersion
    , _branchWithEventsPreviousPackageId = dto ^. previousPackageId
    , _branchWithEventsEvents = fromDTOs $ dto ^. events
    , _branchWithEventsOwnerUuid = dto ^. ownerUuid
    , _branchWithEventsCreatedAt = dto ^. createdAt
    , _branchWithEventsUpdatedAt = dto ^. updatedAt
    }

fromChangeDTO ::
     BranchChangeDTO -> U.UUID -> Int -> Maybe String -> Maybe U.UUID -> UTCTime -> UTCTime -> BranchWithEvents
fromChangeDTO dto bUuid bMetamodelVersion bPackageId mOwnerUuid bCreatedAt bUpdatedAt =
  BranchWithEvents
    { _branchWithEventsUuid = bUuid
    , _branchWithEventsName = dto ^. name
    , _branchWithEventsKmId = dto ^. kmId
    , _branchWithEventsMetamodelVersion = bMetamodelVersion
    , _branchWithEventsPreviousPackageId = bPackageId
    , _branchWithEventsOwnerUuid = mOwnerUuid
    , _branchWithEventsEvents = fromDTOs $ dto ^. events
    , _branchWithEventsCreatedAt = bCreatedAt
    , _branchWithEventsUpdatedAt = bUpdatedAt
    }

fromCreateDTO :: BranchCreateDTO -> U.UUID -> Maybe U.UUID -> UTCTime -> UTCTime -> BranchWithEvents
fromCreateDTO dto bUuid mOwnerUuid bCreatedAt bUpdatedAt =
  BranchWithEvents
    { _branchWithEventsUuid = bUuid
    , _branchWithEventsName = dto ^. name
    , _branchWithEventsKmId = dto ^. kmId
    , _branchWithEventsMetamodelVersion = kmMetamodelVersion
    , _branchWithEventsPreviousPackageId = dto ^. previousPackageId
    , _branchWithEventsOwnerUuid = mOwnerUuid
    , _branchWithEventsEvents = []
    , _branchWithEventsCreatedAt = bCreatedAt
    , _branchWithEventsUpdatedAt = bUpdatedAt
    }
