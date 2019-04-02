module Service.Branch.BranchMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Branch.BranchChangeDTO
import Api.Resource.Branch.BranchCreateDTO
import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchDetailDTO
import Api.Resource.Branch.BranchWithEventsDTO
import Api.Resource.Organization.OrganizationDTO
import LensesConfig
import Model.Branch.Branch
import Model.Branch.BranchState
import Service.Event.EventMapper

toDTO :: BranchWithEvents -> BranchState -> OrganizationDTO -> BranchDTO
toDTO branch state organization =
  BranchDTO
  { _branchDTOUuid = branch ^. uuid
  , _branchDTOName = branch ^. name
  , _branchDTOOrganizationId = organization ^. organizationId
  , _branchDTOKmId = branch ^. kmId
  , _branchDTOState = state
  , _branchDTOParentPackageId = branch ^. parentPackageId
  , _branchDTOLastAppliedParentPackageId = branch ^. lastAppliedParentPackageId
  , _branchDTOOwnerUuid = branch ^. ownerUuid
  , _branchDTOCreatedAt = branch ^. createdAt
  , _branchDTOUpdatedAt = branch ^. updatedAt
  }

toDetailDTO :: BranchWithEvents -> BranchState -> OrganizationDTO -> BranchDetailDTO
toDetailDTO branch state organization =
  BranchDetailDTO
  { _branchDetailDTOUuid = branch ^. uuid
  , _branchDetailDTOName = branch ^. name
  , _branchDetailDTOOrganizationId = organization ^. organizationId
  , _branchDetailDTOKmId = branch ^. kmId
  , _branchDetailDTOState = state
  , _branchDetailDTOParentPackageId = branch ^. parentPackageId
  , _branchDetailDTOLastAppliedParentPackageId = branch ^. lastAppliedParentPackageId
  , _branchDetailDTOEvents = toDTOs $ branch ^. events
  , _branchDetailDTOOwnerUuid = branch ^. ownerUuid
  , _branchDetailDTOCreatedAt = branch ^. createdAt
  , _branchDetailDTOUpdatedAt = branch ^. updatedAt
  }

toWithEventsDTO :: BranchWithEvents -> BranchWithEventsDTO
toWithEventsDTO branch =
  BranchWithEventsDTO
  { _branchWithEventsDTOUuid = branch ^. uuid
  , _branchWithEventsDTOName = branch ^. name
  , _branchWithEventsDTOKmId = branch ^. kmId
  , _branchWithEventsDTOMetamodelVersion = branch ^. metamodelVersion
  , _branchWithEventsDTOParentPackageId = branch ^. parentPackageId
  , _branchWithEventsDTOLastAppliedParentPackageId = branch ^. lastAppliedParentPackageId
  , _branchWithEventsDTOLastMergeCheckpointPackageId = branch ^. lastMergeCheckpointPackageId
  , _branchWithEventsDTOEvents = toDTOs $ branch ^. events
  , _branchWithEventsDTOOwnerUuid = branch ^. ownerUuid
  , _branchWithEventsDTOCreatedAt = branch ^. createdAt
  , _branchWithEventsDTOUpdatedAt = branch ^. updatedAt
  }

fromWithEventsDTO :: BranchWithEventsDTO -> BranchWithEvents
fromWithEventsDTO dto =
  BranchWithEvents
  { _branchWithEventsUuid = dto ^. uuid
  , _branchWithEventsName = dto ^. name
  , _branchWithEventsKmId = dto ^. kmId
  , _branchWithEventsMetamodelVersion = dto ^. metamodelVersion
  , _branchWithEventsParentPackageId = dto ^. parentPackageId
  , _branchWithEventsLastAppliedParentPackageId = dto ^. lastAppliedParentPackageId
  , _branchWithEventsLastMergeCheckpointPackageId = dto ^. lastMergeCheckpointPackageId
  , _branchWithEventsEvents = fromDTOs $ dto ^. events
  , _branchWithEventsOwnerUuid = dto ^. ownerUuid
  , _branchWithEventsCreatedAt = dto ^. createdAt
  , _branchWithEventsUpdatedAt = dto ^. updatedAt
  }

fromChangeDTO ::
     BranchChangeDTO
  -> U.UUID
  -> Int
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> UTCTime
  -> UTCTime
  -> BranchWithEvents
fromChangeDTO dto bUuid bMetamodelVersion bParentPackageId bLastAppliedParentPackageId bLastMergeCheckpointPackageId mOwnerUuid bCreatedAt bUpdatedAt =
  BranchWithEvents
  { _branchWithEventsUuid = bUuid
  , _branchWithEventsName = dto ^. name
  , _branchWithEventsKmId = dto ^. kmId
  , _branchWithEventsMetamodelVersion = bMetamodelVersion
  , _branchWithEventsParentPackageId = bParentPackageId
  , _branchWithEventsLastAppliedParentPackageId = bLastAppliedParentPackageId
  , _branchWithEventsLastMergeCheckpointPackageId = bLastMergeCheckpointPackageId
  , _branchWithEventsOwnerUuid = mOwnerUuid
  , _branchWithEventsEvents = fromDTOs $ dto ^. events
  , _branchWithEventsCreatedAt = bCreatedAt
  , _branchWithEventsUpdatedAt = bUpdatedAt
  }

fromCreateDTO :: BranchCreateDTO -> U.UUID -> Int -> Maybe U.UUID -> UTCTime -> UTCTime -> BranchWithEvents
fromCreateDTO dto bUuid bMetamodelVersion mOwnerUuid bCreatedAt bUpdatedAt =
  BranchWithEvents
  { _branchWithEventsUuid = bUuid
  , _branchWithEventsName = dto ^. name
  , _branchWithEventsKmId = dto ^. kmId
  , _branchWithEventsMetamodelVersion = bMetamodelVersion
  , _branchWithEventsParentPackageId = dto ^. parentPackageId
  , _branchWithEventsLastAppliedParentPackageId = dto ^. parentPackageId
  , _branchWithEventsLastMergeCheckpointPackageId = dto ^. parentPackageId
  , _branchWithEventsOwnerUuid = mOwnerUuid
  , _branchWithEventsEvents = []
  , _branchWithEventsCreatedAt = bCreatedAt
  , _branchWithEventsUpdatedAt = bUpdatedAt
  }
