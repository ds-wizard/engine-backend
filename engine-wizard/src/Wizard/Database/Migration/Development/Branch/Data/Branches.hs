module Wizard.Database.Migration.Development.Branch.Data.Branches where

import Data.Either (rights)
import Data.Maybe (fromJust)
import Data.Time

import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.PackageWithEvents
import qualified Shared.Service.Package.PackageMapper as SPM
import Shared.Util.Uuid
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishBranchDTO
import Wizard.Api.Resource.Package.Publish.PackagePublishMigrationDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.App.App
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchData
import Wizard.Model.Branch.BranchList
import Wizard.Model.Branch.BranchState
import Wizard.Model.User.User
import Wizard.Service.KnowledgeModel.Compilator.Compilator
import qualified Wizard.Service.Package.PackageMapper as PM

amsterdamBranchList :: BranchList
amsterdamBranchList =
  BranchList
    { uuid = u' "6474b24b-262b-42b1-9451-008e8363f2b6"
    , name = amsterdamPackage.name
    , kmId = amsterdamPackage.kmId
    , version = amsterdamPackage.version
    , previousPackageId = Just netherlandsPackage.pId
    , forkOfPackageId = Just netherlandsPackage.pId
    , state = BSEdited
    , createdBy = Just $ userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

amsterdamBranch :: Branch
amsterdamBranch =
  Branch
    { uuid = amsterdamBranchList.uuid
    , name = amsterdamBranchList.name
    , kmId = amsterdamBranchList.kmId
    , version = "1.0.0"
    , description = "First Release"
    , readme = "# Netherlands Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = amsterdamBranchList.previousPackageId
    , createdBy = amsterdamBranchList.createdBy
    , appUuid = defaultApp.uuid
    , createdAt = amsterdamBranchList.createdAt
    , updatedAt = amsterdamBranchList.updatedAt
    }

amsterdamBranchData :: BranchData
amsterdamBranchData =
  BranchData
    { branchUuid = amsterdamBranchList.uuid
    , metamodelVersion = kmMetamodelVersion
    , events =
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
    , appUuid = defaultApp.uuid
    , createdAt = amsterdamBranchList.createdAt
    , updatedAt = amsterdamBranchList.updatedAt
    }

amsterdamBranchCreate :: BranchCreateDTO
amsterdamBranchCreate =
  BranchCreateDTO
    { name = amsterdamBranchList.name
    , kmId = amsterdamBranchList.kmId
    , version = "1.0.0"
    , previousPackageId = amsterdamBranchList.previousPackageId
    }

amsterdamBranchChange :: BranchChangeDTO
amsterdamBranchChange =
  BranchChangeDTO
    { name = "EDITED: " ++ amsterdamBranchList.name
    , kmId = amsterdamBranchList.kmId
    , version = "2.0.0"
    , description = "EDITED: description"
    , readme = "EDITED: Readme"
    , license = "Apacha-3.0"
    }

amsterdamBranchKnowledgeModel :: KnowledgeModel
amsterdamBranchKnowledgeModel =
  head . rights $ [compile Nothing (globalPackage.events ++ netherlandsPackage.events)]

amsterdamBranchDetail :: BranchDetailDTO
amsterdamBranchDetail =
  BranchDetailDTO
    { uuid = amsterdamBranchList.uuid
    , name = amsterdamBranchList.name
    , kmId = amsterdamBranchList.kmId
    , version = amsterdamBranch.version
    , description = amsterdamBranch.description
    , readme = amsterdamBranch.readme
    , license = amsterdamBranch.license
    , state = BSEdited
    , previousPackageId = amsterdamBranchList.previousPackageId
    , forkOfPackageId = amsterdamBranchList.forkOfPackageId
    , forkOfPackage = Just . PM.toSimpleDTO . SPM.toPackage $ netherlandsPackage
    , createdBy = amsterdamBranchList.createdBy
    , events = amsterdamBranchData.events
    , knowledgeModel = amsterdamBranchKnowledgeModel
    , createdAt = amsterdamBranchList.createdAt
    , updatedAt = amsterdamBranchList.updatedAt
    }

leidenBranch :: BranchList
leidenBranch =
  BranchList
    { uuid = u' "47421955-ba30-48d4-8c49-9ec47eda2cad"
    , name = "Leiden KM"
    , kmId = "leiden-km"
    , version = "1.0.0"
    , state = BSDefault
    , previousPackageId = Just netherlandsPackage.pId
    , forkOfPackageId = Just netherlandsPackage.pId
    , createdBy = Just $ userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

leidenBranchCreate :: BranchCreateDTO
leidenBranchCreate =
  BranchCreateDTO
    { name = leidenBranch.name
    , kmId = leidenBranch.kmId
    , version = "1.0.0"
    , previousPackageId = leidenBranch.previousPackageId
    }

differentBranch :: Branch
differentBranch =
  Branch
    { uuid = u' "fc49b6a5-51ae-4442-82e8-c3bf216545ec"
    , name = "Branch Events"
    , kmId = "my-km"
    , version = "1.0.0"
    , description = "Some desc"
    , readme = "Some readme"
    , license = "Apache-2.0"
    , previousPackageId = Just $ differentPackage.pId
    , createdBy = Just $ userCharles.uuid
    , appUuid = differentApp.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentBranchData :: BranchData
differentBranchData =
  BranchData
    { branchUuid = differentBranch.uuid
    , metamodelVersion = kmMetamodelVersion
    , events = []
    , appUuid = differentApp.uuid
    , createdAt = differentBranch.createdAt
    , updatedAt = differentBranch.updatedAt
    }

packagePublishBranchDTO :: PackagePublishBranchDTO
packagePublishBranchDTO =
  PackagePublishBranchDTO
    { branchUuid = amsterdamBranch.uuid
    }

packagePublishMigrationDTO :: PackagePublishMigrationDTO
packagePublishMigrationDTO =
  PackagePublishMigrationDTO
    { branchUuid = amsterdamBranch.uuid
    , version = amsterdamPackage.version
    , description = amsterdamPackage.description
    , readme = amsterdamPackage.readme
    }
