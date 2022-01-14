module Wizard.Database.Migration.Development.Branch.Data.Branches where

import Control.Lens ((^.))
import Data.Either (rights)
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import qualified Shared.Service.Package.PackageMapper as SPM
import Shared.Util.Uuid
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Branch.BranchState
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import qualified Wizard.Service.Package.PackageMapper as PM

amsterdamBranchDto :: BranchDTO
amsterdamBranchDto =
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

amsterdamBranch :: Branch
amsterdamBranch =
  Branch
    { _branchUuid = amsterdamBranchDto ^. uuid
    , _branchName = amsterdamBranchDto ^. name
    , _branchKmId = amsterdamBranchDto ^. kmId
    , _branchPreviousPackageId = amsterdamBranchDto ^. previousPackageId
    , _branchOwnerUuid = amsterdamBranchDto ^. ownerUuid
    , _branchAppUuid = defaultApp ^. uuid
    , _branchCreatedAt = amsterdamBranchDto ^. createdAt
    , _branchUpdatedAt = amsterdamBranchDto ^. updatedAt
    }

amsterdamBranchData :: BranchData
amsterdamBranchData =
  BranchData
    { _branchDataBranchUuid = amsterdamBranchDto ^. uuid
    , _branchDataMetamodelVersion = kmMetamodelVersion
    , _branchDataEvents =
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
    , _branchDataAppUuid = defaultApp ^. uuid
    , _branchDataCreatedAt = amsterdamBranchDto ^. createdAt
    , _branchDataUpdatedAt = amsterdamBranchDto ^. updatedAt
    }

amsterdamBranchCreate :: BranchCreateDTO
amsterdamBranchCreate =
  BranchCreateDTO
    { _branchCreateDTOName = amsterdamBranchDto ^. name
    , _branchCreateDTOKmId = amsterdamBranchDto ^. kmId
    , _branchCreateDTOPreviousPackageId = amsterdamBranchDto ^. previousPackageId
    }

amsterdamBranchChange :: BranchChangeDTO
amsterdamBranchChange =
  BranchChangeDTO
    {_branchChangeDTOName = "EDITED: " ++ amsterdamBranchDto ^. name, _branchChangeDTOKmId = amsterdamBranchDto ^. kmId}

amsterdamBranchKnowledgeModel :: KnowledgeModel
amsterdamBranchKnowledgeModel =
  head . rights $ [compile Nothing ((globalPackage ^. events) ++ (netherlandsPackage ^. events))]

amsterdamBranchDetail :: BranchDetailDTO
amsterdamBranchDetail =
  BranchDetailDTO
    { _branchDetailDTOUuid = amsterdamBranchDto ^. uuid
    , _branchDetailDTOName = amsterdamBranchDto ^. name
    , _branchDetailDTOKmId = amsterdamBranchDto ^. kmId
    , _branchDetailDTOState = BSEdited
    , _branchDetailDTOPreviousPackageId = amsterdamBranchDto ^. previousPackageId
    , _branchDetailDTOForkOfPackageId = amsterdamBranchDto ^. forkOfPackageId
    , _branchDetailDTOForkOfPackage = Just . PM.toSimpleDTO . SPM.toPackage $ netherlandsPackage
    , _branchDetailDTOOwnerUuid = amsterdamBranchDto ^. ownerUuid
    , _branchDetailDTOEvents = amsterdamBranchData ^. events
    , _branchDetailDTOKnowledgeModel = amsterdamBranchKnowledgeModel
    , _branchDetailDTOCreatedAt = amsterdamBranchDto ^. createdAt
    , _branchDetailDTOUpdatedAt = amsterdamBranchDto ^. updatedAt
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

differentBranch :: Branch
differentBranch =
  Branch
    { _branchUuid = u' "fc49b6a5-51ae-4442-82e8-c3bf216545ec"
    , _branchName = "Branch Events"
    , _branchKmId = "my-km"
    , _branchPreviousPackageId = Just $ differentPackage ^. pId
    , _branchOwnerUuid = Just $ userCharles ^. uuid
    , _branchAppUuid = differentApp ^. uuid
    , _branchCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , _branchUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentBranchData :: BranchData
differentBranchData =
  BranchData
    { _branchDataBranchUuid = differentBranch ^. uuid
    , _branchDataMetamodelVersion = kmMetamodelVersion
    , _branchDataEvents = []
    , _branchDataAppUuid = differentApp ^. uuid
    , _branchDataCreatedAt = differentBranch ^. createdAt
    , _branchDataUpdatedAt = differentBranch ^. updatedAt
    }
