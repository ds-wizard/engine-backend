module Wizard.Database.Migration.Development.Branch.Data.Branches where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Event.Event
import Shared.Util.Uuid
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchState

amsterdamBranch :: BranchDTO
amsterdamBranch =
  BranchDTO
    { _branchDTOUuid = u' "6474b24b-262b-42b1-9451-008e8363f2b6"
    , _branchDTOName = amsterdamPackage ^. name
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
    , _branchWithEventsAppUuid = defaultApp ^. uuid
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
    , _branchDetailDTOKmId = amsterdamBranch ^. kmId
    , _branchDetailDTOState = BSEdited
    , _branchDetailDTOPreviousPackageId = amsterdamBranch ^. previousPackageId
    , _branchDetailDTOForkOfPackageId = amsterdamBranch ^. forkOfPackageId
    , _branchDetailDTOOwnerUuid = amsterdamBranch ^. ownerUuid
    , _branchDetailDTOEvents = amsterdamBranchWithEvents ^. events
    , _branchDetailDTOCreatedAt = amsterdamBranch ^. createdAt
    , _branchDetailDTOUpdatedAt = amsterdamBranch ^. updatedAt
    }

leidenBranch :: BranchDTO
leidenBranch =
  BranchDTO
    { _branchDTOUuid = u' "47421955-ba30-48d4-8c49-9ec47eda2cad"
    , _branchDTOName = "Leiden KM"
    , _branchDTOKmId = "leiden-km"
    , _branchDTOState = BSDefault
    , _branchDTOPreviousPackageId = Just $ netherlandsPackage ^. pId
    , _branchDTOForkOfPackageId = Just $ netherlandsPackage ^. pId
    , _branchDTOOwnerUuid = Just $ userAlbert ^. uuid
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

differentBranch :: BranchWithEvents
differentBranch =
  BranchWithEvents
    { _branchWithEventsUuid = u' "fc49b6a5-51ae-4442-82e8-c3bf216545ec"
    , _branchWithEventsName = "Branch Events"
    , _branchWithEventsKmId = "my-km"
    , _branchWithEventsMetamodelVersion = kmMetamodelVersion
    , _branchWithEventsPreviousPackageId = Just $ differentPackage ^. pId
    , _branchWithEventsEvents = []
    , _branchWithEventsOwnerUuid = Just $ userCharles ^. uuid
    , _branchWithEventsAppUuid = differentApp ^. uuid
    , _branchWithEventsCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _branchWithEventsUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }
