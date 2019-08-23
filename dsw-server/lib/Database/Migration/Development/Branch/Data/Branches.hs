module Database.Migration.Development.Branch.Data.Branches where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Branch.BranchChangeDTO
import Api.Resource.Branch.BranchCreateDTO
import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchDetailDTO
import Constant.KnowledgeModel
import Database.Migration.Development.Event.Data.Events
import Database.Migration.Development.Organization.Data.Organizations
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.User.Data.Users
import LensesConfig
import Model.Branch.Branch
import Model.Branch.BranchState
import Model.Event.Event
import Service.Event.EventMapper

amsterdamBranch :: BranchDTO
amsterdamBranch =
  BranchDTO
  { _branchDTOUuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
  , _branchDTOName = amsterdamPackage ^. name
  , _branchDTOOrganizationId = org1 ^. organizationId
  , _branchDTOKmId = amsterdamPackage ^. kmId
  , _branchDTOPreviousPackageId = Just $ netherlandsPackage ^. pId
  , _branchDTOForkOfPackageId = Just $ netherlandsPackage ^. pId
  , _branchDTOState = BSEdited
  , _branchDTOOwnerUuid = Just $ userAlbert ^. uuid
  , _branchDTOCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  , _branchDTOUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

amsterdamBranchWithEvents :: BranchWithEvents
amsterdamBranchWithEvents =
  BranchWithEvents
  { _branchWithEventsUuid = amsterdamBranch ^. uuid
  , _branchWithEventsName = amsterdamBranch ^. name
  , _branchWithEventsKmId = amsterdamBranch ^. kmId
  , _branchWithEventsMetamodelVersion = kmMetamodelVersion
  , _branchWithEventsPreviousPackageId = amsterdamBranch ^. previousPackageId
  , _branchWithEventsOwnerUuid = amsterdamBranch ^. ownerUuid
  , _branchWithEventsEvents =
      [ AddQuestionEvent' a_km1_ch1_q1'
      , AddQuestionEvent' a_km1_ch1_q2'
      , AddAnswerEvent' a_km1_ch1_q2_aNo1
      , AddAnswerEvent' a_km1_ch1_q2_aYes1
      , AddQuestionEvent' a_km1_ch1_ansYes1_fuq1'
      , AddAnswerEvent' a_km1_ch1_q2_aYes1_fuq1_aNo
      , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
      , AddQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2'
      , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
      , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
      , AddExpertEvent' a_km1_ch1_q2_eAlbert
      , AddExpertEvent' a_km1_ch1_q2_eNikola
      , AddReferenceEvent' a_km1_ch1_q2_rCh1'
      , AddReferenceEvent' a_km1_ch1_q2_rCh2'
      , AddChapterEvent' a_km1_ch2
      , AddQuestionEvent' a_km1_ch2_q3'
      , AddAnswerEvent' a_km1_ch2_q3_aNo2
      , AddAnswerEvent' a_km1_ch2_q3_aYes2
      ]
  , _branchWithEventsCreatedAt = amsterdamBranch ^. createdAt
  , _branchWithEventsUpdatedAt = amsterdamBranch ^. updatedAt
  }

amsterdamBranchCreate :: BranchCreateDTO
amsterdamBranchCreate =
  BranchCreateDTO
  { _branchCreateDTOName = amsterdamBranch ^. name
  , _branchCreateDTOKmId = amsterdamBranch ^. kmId
  , _branchCreateDTOPreviousPackageId = amsterdamBranch ^. previousPackageId
  }

amsterdamBranchChange :: BranchChangeDTO
amsterdamBranchChange =
  BranchChangeDTO
  { _branchChangeDTOName = "EDITED: " ++ amsterdamBranch ^. name
  , _branchChangeDTOKmId = amsterdamBranch ^. kmId
  , _branchChangeDTOEvents = []
  }

amsterdamBranchDetail :: BranchDetailDTO
amsterdamBranchDetail =
  BranchDetailDTO
  { _branchDetailDTOUuid = amsterdamBranch ^. uuid
  , _branchDetailDTOName = amsterdamBranch ^. name
  , _branchDetailDTOOrganizationId = amsterdamBranch ^. organizationId
  , _branchDetailDTOKmId = amsterdamBranch ^. kmId
  , _branchDetailDTOState = BSEdited
  , _branchDetailDTOPreviousPackageId = amsterdamBranch ^. previousPackageId
  , _branchDetailDTOForkOfPackageId = amsterdamBranch ^. forkOfPackageId
  , _branchDetailDTOOwnerUuid = amsterdamBranch ^. ownerUuid
  , _branchDetailDTOEvents = toDTOs $ amsterdamBranchWithEvents ^. events
  , _branchDetailDTOCreatedAt = amsterdamBranch ^. createdAt
  , _branchDetailDTOUpdatedAt = amsterdamBranch ^. updatedAt
  }

leidenBranch :: BranchDTO
leidenBranch =
  BranchDTO
  { _branchDTOUuid = fromJust (U.fromString "47421955-ba30-48d4-8c49-9ec47eda2cad")
  , _branchDTOName = "Leiden KM"
  , _branchDTOOrganizationId = org1 ^. organizationId
  , _branchDTOKmId = "leiden-km"
  , _branchDTOState = BSDefault
  , _branchDTOPreviousPackageId = Just $ netherlandsPackage ^. pId
  , _branchDTOForkOfPackageId = Just $ netherlandsPackage ^. pId
  , _branchDTOOwnerUuid = Just $ fromJust (U.fromString "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66")
  , _branchDTOCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  , _branchDTOUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

leidenBranchCreate :: BranchCreateDTO
leidenBranchCreate =
  BranchCreateDTO
  { _branchCreateDTOName = leidenBranch ^. name
  , _branchCreateDTOKmId = leidenBranch ^. kmId
  , _branchCreateDTOPreviousPackageId = leidenBranch ^. previousPackageId
  }
