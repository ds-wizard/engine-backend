module Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages where

import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageGroup
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

globalKmPackageEmpty :: KnowledgeModelPackage
globalKmPackageEmpty =
  KnowledgeModelPackage
    { pId = "global:core:0.0.1"
    , name = "Global Knowledge Model"
    , organizationId = "global"
    , kmId = "core"
    , version = "0.0.1"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "Empty package"
    , readme = "# Global Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Nothing
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , nonEditable = False
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

globalKmPackageEmptyEvents :: [KnowledgeModelPackageEvent]
globalKmPackageEmptyEvents =
  fmap
    (toPackageEvent globalKmPackageEmpty.pId U.nil)
    [a_km1_ch1_q1]

globalKmPackage :: KnowledgeModelPackage
globalKmPackage =
  KnowledgeModelPackage
    { pId = "global:core:1.0.0"
    , name = "Global Knowledge Model"
    , organizationId = "global"
    , kmId = "core"
    , version = "1.0.0"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "First Release"
    , readme = "# Global Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Nothing
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , nonEditable = False
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

globalKmPackageEvents :: [KnowledgeModelPackageEvent]
globalKmPackageEvents =
  fmap
    (toPackageEvent globalKmPackage.pId U.nil)
    [ a_km1
    , a_km1_mtrF
    , a_km1_mtrA
    , a_km1_mtrI
    , a_km1_mtrR
    , a_km1_mtrG
    , a_km1_mtrO
    , a_km1_phs1
    , a_km1_phs2
    , a_km1_phs3
    , a_km1_tds
    , a_km1_tbi
    , a_km1_iop
    , a_km1_ibp
    , a_km1_iwp
    , a_km1_rc1
    , a_km1_rc1_rp1
    , a_km1_rc1_rp2
    , a_km1_rc2
    , a_km1_rc2_rp1
    ]

globalKmPackageDeprecated :: KnowledgeModelPackage
globalKmPackageDeprecated =
  globalKmPackage
    { phase = DeprecatedKnowledgeModelPackagePhase
    }

globalKmPackageGroup :: KnowledgeModelPackageGroup
globalKmPackageGroup =
  KnowledgeModelPackageGroup
    { organizationId = globalKmPackage.organizationId
    , kmId = globalKmPackage.kmId
    , versions = L.intercalate "," [globalKmPackageEmpty.version, globalKmPackage.version]
    }

netherlandsKmPackage :: KnowledgeModelPackage
netherlandsKmPackage =
  KnowledgeModelPackage
    { pId = "org.nl:core-nl:1.0.0"
    , name = "Netherlands Knowledge Model"
    , organizationId = "org.nl"
    , kmId = "core-nl"
    , version = "1.0.0"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "First Release"
    , readme = "# Netherlands Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Just $ globalKmPackage.pId
    , forkOfPackageId = Just $ globalKmPackage.pId
    , mergeCheckpointPackageId = Just $ globalKmPackage.pId
    , nonEditable = False
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

netherlandsKmPackageEvents :: [KnowledgeModelPackageEvent]
netherlandsKmPackageEvents =
  fmap
    (toPackageEvent netherlandsKmPackage.pId U.nil)
    [a_km1_ch1]

netherlandsKmPackageV2 :: KnowledgeModelPackage
netherlandsKmPackageV2 =
  KnowledgeModelPackage
    { pId = "org.nl:core-nl:2.0.0"
    , name = "Netherlands Knowledge Model"
    , organizationId = "org.nl"
    , kmId = "core-nl"
    , version = "2.0.0"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "Second Release"
    , readme = "# Netherlands Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Just netherlandsKmPackage.pId
    , forkOfPackageId = Just $ globalKmPackage.pId
    , mergeCheckpointPackageId = Just $ globalKmPackage.pId
    , nonEditable = False
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

netherlandsKmPackageV2Events :: [KnowledgeModelPackageEvent]
netherlandsKmPackageV2Events =
  fmap
    (toPackageEvent netherlandsKmPackageV2.pId U.nil)
    [a_km1_ch4]

amsterdamKmPackage :: KnowledgeModelPackage
amsterdamKmPackage =
  KnowledgeModelPackage
    { pId = "org.nl.amsterdam:core-amsterdam:1.0.0"
    , name = "Amsterdam Knowledge Model"
    , organizationId = "org.nl.amsterdam"
    , kmId = "core-amsterdam"
    , version = "1.0.0"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "First Release"
    , readme = "# Amsterdam Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Just netherlandsKmPackage.pId
    , forkOfPackageId = Just netherlandsKmPackage.pId
    , mergeCheckpointPackageId = Just netherlandsKmPackage.pId
    , nonEditable = False
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

germanyKmPackage :: KnowledgeModelPackage
germanyKmPackage =
  KnowledgeModelPackage
    { pId = "org.de:core-de:1.0.0"
    , name = "Germany Knowledge Model"
    , organizationId = "org.de"
    , kmId = "core-de"
    , version = "1.0.0"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = kmMetamodelVersion
    , description = "First Release"
    , readme = "# Germany Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = Just $ globalKmPackageEmpty.pId
    , forkOfPackageId = Just $ globalKmPackageEmpty.pId
    , mergeCheckpointPackageId = Just $ globalKmPackageEmpty.pId
    , nonEditable = False
    , tenantUuid = U.nil
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

germanyKmPackageEvents :: [KnowledgeModelPackageEvent]
germanyKmPackageEvents =
  fmap
    (toPackageEvent germanyKmPackage.pId U.nil)
    [ a_km1
    , a_km1_mtrF
    , a_km1_mtrA
    , a_km1_mtrI
    , a_km1_mtrR
    , a_km1_mtrG
    , a_km1_mtrO
    , a_km1_phs1
    , a_km1_phs2
    , a_km1_phs3
    , a_km1_tds
    , a_km1_tbi
    , a_km1_ir
    , a_km1_iop
    , a_km1_ibp
    , a_km1_iwp
    , a_km1_rc1
    , a_km1_rc1_rp1
    , a_km1_rc1_rp2
    , a_km1_rc2
    , a_km1_rc2_rp1
    , a_km1_ch1
    , a_km1_ch1_q1
    , a_km1_ch1_q2
    , a_km1_ch1_q2_aNo1
    , a_km1_ch1_q2_aYes1
    , a_km1_ch1_ansYes1_fuq1
    , a_km1_ch1_q2_aYes1_fuq1_aNo
    , a_km1_ch1_q2_aYesFu1
    , a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2
    , a_km1_ch1_q2_aNoFu2
    , a_km1_ch1_q2_aYesFu2
    , a_km1_ch1_q2_eAlbert
    , a_km1_ch1_q2_eNikola
    , a_km1_ch1_q2_rCh1
    , a_km1_ch1_q2_rCh2
    , a_km1_ch2
    , a_km1_ch2_q3
    , a_km1_ch2_q3_aNo2
    , a_km1_ch2_q3_aYes2
    , a_km1_ch2_q4
    , a_km1_ch2_q4_it1_q5
    , a_km1_ch2_q4_it1_q7
    , a_km1_ch2_q4_it1_q8
    , a_km1_ch2_q4_it1_q6
    , a_km1_ch2_q4_it_q6_aNo
    , a_km1_ch2_q4_it_q6_aYes
    , a_km1_ch2_ansYes6_fuq4
    , a_km1_ch2_ansYes6_fuq5
    , a_km1_ch2_q4_it1_q6_fuq4_q1
    , a_km1_ch2_q4_it1_q6_fuq4_q2
    , a_km1_ch2_q6_eAlbert
    , a_km1_ch2_q6_eNikola
    , a_km1_ch2_q6_rCh1
    , a_km1_ch2_q6_rCh2
    , a_km1_ch3
    , a_km1_ch3_q9
    , a_km1_ch3_q10
    , a_km1_ch3_q11
    , a_km1_ch3_q11_cho1
    , a_km1_ch3_q11_cho2
    , a_km1_ch3_q12
    , a_km1_ch3_q13
    , a_km1_ch3_q14
    , a_km1_ch3_q15
    ]

germanyEvents :: [KnowledgeModelEvent]
germanyEvents = fmap toEvent germanyKmPackageEvents

kmPackagePatternAll :: KnowledgeModelPackagePattern
kmPackagePatternAll =
  KnowledgeModelPackagePattern
    { orgId = Nothing
    , kmId = Nothing
    , minVersion = Nothing
    , maxVersion = Nothing
    }

kmPackagePatternAllEdited :: KnowledgeModelPackagePattern
kmPackagePatternAllEdited =
  KnowledgeModelPackagePattern
    { orgId = Just "global"
    , kmId = Nothing
    , minVersion = Nothing
    , maxVersion = Nothing
    }

kmPackagePatternGlobal :: KnowledgeModelPackagePattern
kmPackagePatternGlobal =
  KnowledgeModelPackagePattern
    { orgId = Just "global"
    , kmId = Just "core"
    , minVersion = Just "1.0.0"
    , maxVersion = Just "1.0.0"
    }
