module Database.Migration.Development.Package.Data.Packages where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import Constant.KnowledgeModel
import Database.Migration.Development.Event.Data.Events
import Database.Migration.Development.Organization.Data.Organizations
import Integration.Resource.Organization.OrganizationSimpleIDTO
import Integration.Resource.Package.PackageSimpleIDTO
import LensesConfig
import Model.Event.Event
import Model.Package.PackageWithEvents

globalPackageEmpty :: PackageWithEvents
globalPackageEmpty =
  PackageWithEvents
  { _packageWithEventsPId = "dsw:core:0.0.1"
  , _packageWithEventsName = "DSW Global Knowledge Model"
  , _packageWithEventsOrganizationId = "dsw"
  , _packageWithEventsKmId = "core"
  , _packageWithEventsVersion = "0.0.1"
  , _packageWithEventsMetamodelVersion = kmMetamodelVersion
  , _packageWithEventsDescription = "Empty package"
  , _packageWithEventsReadme = "**There should be README. For changing it, publish a new version.**"
  , _packageWithEventsLicense = "## License\\n There should be some license"
  , _packageWithEventsParentPackageId = Nothing
  , _packageWithEventsEvents = []
  , _packageWithEventsCreatedAt = (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)
  }

globalPackage :: PackageWithEvents
globalPackage =
  PackageWithEvents
  { _packageWithEventsPId = "dsw:core:1.0.0"
  , _packageWithEventsName = "DSW Global Knowledge Model"
  , _packageWithEventsOrganizationId = "dsw"
  , _packageWithEventsKmId = "core"
  , _packageWithEventsVersion = "1.0.0"
  , _packageWithEventsMetamodelVersion = kmMetamodelVersion
  , _packageWithEventsDescription = "First Release"
  , _packageWithEventsReadme = "**There should be README. For changing it, publish a new version.**"
  , _packageWithEventsLicense = "## License\\n There should be some license"
  , _packageWithEventsParentPackageId = Nothing
  , _packageWithEventsEvents =
      [ AddKnowledgeModelEvent' a_km1
      , AddTagEvent' a_km1_tds
      , AddTagEvent' a_km1_tbi
      , AddIntegrationEvent' a_km1_iop
      , AddIntegrationEvent' a_km1_ibp
      ]
  , _packageWithEventsCreatedAt = (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)
  }

netherlandsPackage :: PackageWithEvents
netherlandsPackage =
  PackageWithEvents
  { _packageWithEventsPId = "dsw.nl:core-nl:1.0.0"
  , _packageWithEventsName = "DSW Netherlands Knowledge Model"
  , _packageWithEventsOrganizationId = "dsw.nl"
  , _packageWithEventsKmId = "core-nl"
  , _packageWithEventsVersion = "1.0.0"
  , _packageWithEventsMetamodelVersion = kmMetamodelVersion
  , _packageWithEventsDescription = "First Release"
  , _packageWithEventsReadme = "**There should be README. For changing it, publish a new version.**"
  , _packageWithEventsLicense = "## License\\n There should be some license"
  , _packageWithEventsParentPackageId = (Just $ globalPackage ^. pId)
  , _packageWithEventsEvents = [AddChapterEvent' a_km1_ch1]
  , _packageWithEventsCreatedAt = (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)
  }

netherlandsPackageV2 :: PackageWithEvents
netherlandsPackageV2 =
  PackageWithEvents
  { _packageWithEventsPId = "dsw.nl:core-nl:2.0.0"
  , _packageWithEventsName = "DSW Netherlands Knowledge Model"
  , _packageWithEventsOrganizationId = "dsw.nl"
  , _packageWithEventsKmId = "core-nl"
  , _packageWithEventsVersion = "2.0.0"
  , _packageWithEventsMetamodelVersion = kmMetamodelVersion
  , _packageWithEventsDescription = "Second Release"
  , _packageWithEventsReadme = "**There should be README. For changing it, publish a new version.**"
  , _packageWithEventsLicense = "## License\\n There should be some license"
  , _packageWithEventsParentPackageId = (Just $ netherlandsPackage ^. pId)
  , _packageWithEventsEvents = [AddChapterEvent' a_km1_ch4]
  , _packageWithEventsCreatedAt = (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)
  }

amsterdamPackage :: PackageWithEvents
amsterdamPackage =
  PackageWithEvents
  { _packageWithEventsPId = "dsw.nl.amsterdam:core-amsterdam:1.0.0"
  , _packageWithEventsName = "DSW Amsterdam Knowledge Model"
  , _packageWithEventsOrganizationId = "dsw.nl.amsterdam"
  , _packageWithEventsKmId = "core-amsterdam"
  , _packageWithEventsVersion = "1.0.0"
  , _packageWithEventsMetamodelVersion = kmMetamodelVersion
  , _packageWithEventsDescription = "First Release"
  , _packageWithEventsReadme = "**There should be README. For changing it, publish a new version.**"
  , _packageWithEventsLicense = "## License\\n There should be some license"
  , _packageWithEventsParentPackageId = (Just $ netherlandsPackage ^. pId)
  , _packageWithEventsEvents = []
  , _packageWithEventsCreatedAt = (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)
  }

germanyPackage :: PackageWithEvents
germanyPackage =
  PackageWithEvents
  { _packageWithEventsPId = "dsw.de:core-de:1.0.0"
  , _packageWithEventsName = "DSW Germany"
  , _packageWithEventsOrganizationId = "dsw.de"
  , _packageWithEventsKmId = "core-de"
  , _packageWithEventsVersion = "1.0.0"
  , _packageWithEventsMetamodelVersion = kmMetamodelVersion
  , _packageWithEventsDescription = "First Release"
  , _packageWithEventsReadme = "**There should be README. For changing it, publish a new version.**"
  , _packageWithEventsLicense = "## License\\n There should be some license"
  , _packageWithEventsParentPackageId = (Just $ globalPackageEmpty ^. pId)
  , _packageWithEventsEvents =
      [ AddKnowledgeModelEvent' a_km1
      , AddTagEvent' a_km1_tds
      , AddTagEvent' a_km1_tbi
      , AddIntegrationEvent' a_km1_iop
      , AddIntegrationEvent' a_km1_ibp
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
      ]
  , _packageWithEventsCreatedAt = (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)
  }

globalRemotePackage :: PackageSimpleIDTO
globalRemotePackage =
  PackageSimpleIDTO
  { _packageSimpleIDTOPId = globalPackage ^. pId
  , _packageSimpleIDTOName = globalPackage ^. name
  , _packageSimpleIDTOOrganizationId = globalPackage ^. organizationId
  , _packageSimpleIDTOKmId = globalPackage ^. kmId
  , _packageSimpleIDTOVersion = globalPackage ^. version
  , _packageSimpleIDTODescription = globalPackage ^. description
  , _packageSimpleIDTOOrganization =
      OrganizationSimpleIDTO
      { _organizationSimpleIDTOOrganizationId = orgDsw ^. organizationId
      , _organizationSimpleIDTOName = orgDsw ^. name
      , _organizationSimpleIDTOLogo = Just orgLogo
      }
  , _packageSimpleIDTOCreatedAt = globalPackage ^. createdAt
  }

globalNetherlandsPackage :: PackageSimpleIDTO
globalNetherlandsPackage =
  PackageSimpleIDTO
  { _packageSimpleIDTOPId = netherlandsPackageV2 ^. pId
  , _packageSimpleIDTOName = netherlandsPackageV2 ^. name
  , _packageSimpleIDTOOrganizationId = netherlandsPackageV2 ^. organizationId
  , _packageSimpleIDTOKmId = netherlandsPackageV2 ^. kmId
  , _packageSimpleIDTOVersion = netherlandsPackageV2 ^. version
  , _packageSimpleIDTODescription = netherlandsPackageV2 ^. description
  , _packageSimpleIDTOOrganization =
      OrganizationSimpleIDTO
      { _organizationSimpleIDTOOrganizationId = orgNetherlands ^. organizationId
      , _organizationSimpleIDTOName = orgNetherlands ^. name
      , _organizationSimpleIDTOLogo = Just orgLogo
      }
  , _packageSimpleIDTOCreatedAt = globalPackage ^. createdAt
  }

orgLogo :: String
orgLogo =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+P+/HgAFhAJ/wlseKgAAAABJRU5ErkJggg=="
