module Shared.Database.Migration.Development.Package.Data.Packages where

import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Package.PackageDTO
import Shared.Constant.KnowledgeModel
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Event
import Shared.Model.Package.Package
import Shared.Model.Package.PackageGroup
import Shared.Model.Package.PackagePattern
import Shared.Model.Package.PackageWithEvents
import Shared.Service.Package.PackageMapper

globalPackageEmpty :: PackageWithEvents
globalPackageEmpty =
  PackageWithEvents
    { pId = "global:core:0.0.1"
    , name = "Global Knowledge Model"
    , organizationId = "global"
    , kmId = "core"
    , version = "0.0.1"
    , phase = ReleasedPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "Empty package"
    , readme = "# Global Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Nothing
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , events = [AddQuestionEvent' a_km1_ch1_q1']
    , appUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

globalPackage :: PackageWithEvents
globalPackage =
  PackageWithEvents
    { pId = "global:core:1.0.0"
    , name = "Global Knowledge Model"
    , organizationId = "global"
    , kmId = "core"
    , version = "1.0.0"
    , phase = ReleasedPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "First Release"
    , readme = "# Global Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Nothing
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , events =
        [ AddKnowledgeModelEvent' a_km1
        , AddMetricEvent' a_km1_mtrF
        , AddMetricEvent' a_km1_mtrA
        , AddMetricEvent' a_km1_mtrI
        , AddMetricEvent' a_km1_mtrR
        , AddMetricEvent' a_km1_mtrG
        , AddMetricEvent' a_km1_mtrO
        , AddPhaseEvent' a_km1_phs1
        , AddPhaseEvent' a_km1_phs2
        , AddPhaseEvent' a_km1_phs3
        , AddTagEvent' a_km1_tds
        , AddTagEvent' a_km1_tbi
        , AddIntegrationEvent' a_km1_iop'
        , AddIntegrationEvent' a_km1_ibp'
        , AddIntegrationEvent' a_km1_iwp'
        ]
    , appUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

globalPackageDeprecated :: PackageWithEvents
globalPackageDeprecated =
  globalPackage
    { phase = DeprecatedPackagePhase
    }

globalPackageDto :: PackageDTO
globalPackageDto = toDTO globalPackage

globalPackageGroup :: PackageGroup
globalPackageGroup =
  PackageGroup
    { organizationId = globalPackage.organizationId
    , kmId = globalPackage.kmId
    , versions = L.intercalate "," [globalPackageEmpty.version, globalPackage.version]
    }

netherlandsPackage :: PackageWithEvents
netherlandsPackage =
  PackageWithEvents
    { pId = "org.nl:core-nl:1.0.0"
    , name = "Netherlands Knowledge Model"
    , organizationId = "org.nl"
    , kmId = "core-nl"
    , version = "1.0.0"
    , phase = ReleasedPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "First Release"
    , readme = "# Netherlands Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Just $ globalPackage.pId
    , forkOfPackageId = Just $ globalPackage.pId
    , mergeCheckpointPackageId = Just $ globalPackage.pId
    , events = [AddChapterEvent' a_km1_ch1]
    , appUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

netherlandsPackageV2 :: PackageWithEvents
netherlandsPackageV2 =
  PackageWithEvents
    { pId = "org.nl:core-nl:2.0.0"
    , name = "Netherlands Knowledge Model"
    , organizationId = "org.nl"
    , kmId = "core-nl"
    , version = "2.0.0"
    , phase = ReleasedPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "Second Release"
    , readme = "# Netherlands Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Just netherlandsPackage.pId
    , forkOfPackageId = Just $ globalPackage.pId
    , mergeCheckpointPackageId = Just $ globalPackage.pId
    , events = [AddChapterEvent' a_km1_ch4]
    , appUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

netherlandsPackageGroup :: PackageGroup
netherlandsPackageGroup =
  PackageGroup
    { organizationId = netherlandsPackageV2.organizationId
    , kmId = netherlandsPackageV2.kmId
    , versions = L.intercalate "," [netherlandsPackage.version, netherlandsPackageV2.version]
    }

amsterdamPackage :: PackageWithEvents
amsterdamPackage =
  PackageWithEvents
    { pId = "org.nl.amsterdam:core-amsterdam:1.0.0"
    , name = "Amsterdam Knowledge Model"
    , organizationId = "org.nl.amsterdam"
    , kmId = "core-amsterdam"
    , version = "1.0.0"
    , phase = ReleasedPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "First Release"
    , readme = "# Amsterdam Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Just netherlandsPackage.pId
    , forkOfPackageId = Just netherlandsPackage.pId
    , mergeCheckpointPackageId = Just netherlandsPackage.pId
    , events = []
    , appUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

germanyPackage :: PackageWithEvents
germanyPackage =
  PackageWithEvents
    { pId = "org.de:core-de:1.0.0"
    , name = "Germany Knowledge Model"
    , organizationId = "org.de"
    , kmId = "core-de"
    , version = "1.0.0"
    , phase = ReleasedPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "First Release"
    , readme = "# Germany Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Just $ globalPackageEmpty.pId
    , forkOfPackageId = Just $ globalPackageEmpty.pId
    , mergeCheckpointPackageId = Just $ globalPackageEmpty.pId
    , events =
        [ AddKnowledgeModelEvent' a_km1
        , AddMetricEvent' a_km1_mtrF
        , AddMetricEvent' a_km1_mtrA
        , AddMetricEvent' a_km1_mtrI
        , AddMetricEvent' a_km1_mtrR
        , AddMetricEvent' a_km1_mtrG
        , AddMetricEvent' a_km1_mtrO
        , AddPhaseEvent' a_km1_phs1
        , AddPhaseEvent' a_km1_phs2
        , AddPhaseEvent' a_km1_phs3
        , AddTagEvent' a_km1_tds
        , AddTagEvent' a_km1_tbi
        , AddIntegrationEvent' a_km1_iop'
        , AddIntegrationEvent' a_km1_ibp'
        , AddIntegrationEvent' a_km1_iwp'
        , AddChapterEvent' a_km1_ch1
        , AddQuestionEvent' a_km1_ch1_q1'
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
        , AddQuestionEvent' a_km1_ch2_q4'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q5'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q7'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q8'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q6'
        , AddAnswerEvent' a_km1_ch2_q4_it_q6_aNo
        , AddAnswerEvent' a_km1_ch2_q4_it_q6_aYes
        , AddQuestionEvent' a_km1_ch2_ansYes6_fuq4'
        , AddQuestionEvent' a_km1_ch2_ansYes6_fuq5'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q1'
        , AddQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q2'
        , AddExpertEvent' a_km1_ch2_q6_eAlbert
        , AddExpertEvent' a_km1_ch2_q6_eNikola
        , AddReferenceEvent' a_km1_ch2_q6_rCh1'
        , AddReferenceEvent' a_km1_ch2_q6_rCh2'
        , AddChapterEvent' a_km1_ch3
        , AddQuestionEvent' a_km1_ch3_q9'
        , AddQuestionEvent' a_km1_ch3_q10'
        , AddQuestionEvent' a_km1_ch3_q11'
        , AddChoiceEvent' a_km1_ch3_q11_cho1
        , AddChoiceEvent' a_km1_ch3_q11_cho2
        , AddQuestionEvent' a_km1_ch3_q12'
        ]
    , appUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

germanyPackageGroup :: PackageGroup
germanyPackageGroup =
  PackageGroup
    { organizationId = germanyPackage.organizationId
    , kmId = germanyPackage.kmId
    , versions = L.intercalate "," [germanyPackage.version]
    }

packagePatternAll :: PackagePattern
packagePatternAll =
  PackagePattern
    { orgId = Nothing
    , kmId = Nothing
    , minVersion = Nothing
    , maxVersion = Nothing
    }

packagePatternAllEdited :: PackagePattern
packagePatternAllEdited =
  PackagePattern
    { orgId = Just "global"
    , kmId = Nothing
    , minVersion = Nothing
    , maxVersion = Nothing
    }

packagePatternGlobal :: PackagePattern
packagePatternGlobal =
  PackagePattern
    { orgId = Just "global"
    , kmId = Just "core"
    , minVersion = Just "1.0.0"
    , maxVersion = Just "1.0.0"
    }
