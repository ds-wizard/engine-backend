module Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.Acl.Data.Members
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.QuestionnaireMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

_QUESTIONNAIRE_PROJECT_TAG_1 = "projectTag1"

_QUESTIONNAIRE_PROJECT_TAG_2 = "projectTag2"

questionnaire1 :: Questionnaire
questionnaire1 =
  Questionnaire
    { uuid = fromJust (U.fromString "af984a75-56e3-49f8-b16f-d6b99599910a")
    , name = "My Private Questionnaire"
    , description = Just "Some description"
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , packageId = germanyPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = [_QUESTIONNAIRE_PROJECT_TAG_1]
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , permissions = [qtn1AlbertEditPermRecord]
    , events = fEvents
    , versions = qVersions
    , isTemplate = True
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire1Edited :: Questionnaire
questionnaire1Edited =
  questionnaire1
    { name = "EDITED: " ++ questionnaire1.name
    , visibility = VisibleEditQuestionnaire
    , sharing = RestrictedQuestionnaire
    , projectTags = [_QUESTIONNAIRE_PROJECT_TAG_1, _QUESTIONNAIRE_PROJECT_TAG_2]
    , permissions = []
    }

questionnaire1ContentEdited :: Questionnaire
questionnaire1ContentEdited = questionnaire1 {events = fEventsEdited}

questionnaire1Ctn :: QuestionnaireContent
questionnaire1Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

questionnaire1CtnRevertedDto :: QuestionnaireContentDTO
questionnaire1CtnRevertedDto =
  QuestionnaireContentDTO
    { phaseUuid = Nothing
    , replies = M.fromList [rQ1, rQ2]
    , commentThreadsMap = qtnThreadsDto
    , labels = M.empty
    , events = [toEventDTO sre_rQ1' (Just userAlbert), toEventDTO sre_rQ2' (Just userAlbert)]
    , versions = []
    }

questionnaire1Dto :: QuestionnaireDTO
questionnaire1Dto = toSimpleDTO questionnaire1 germanyPackage QSDefault [qtn1AlbertEditPermRecordDto]

questionnaire1Create :: QuestionnaireCreateDTO
questionnaire1Create =
  QuestionnaireCreateDTO
    { name = questionnaire1.name
    , packageId = questionnaire1.packageId
    , visibility = questionnaire1.visibility
    , sharing = questionnaire1.sharing
    , questionTagUuids = []
    , documentTemplateId = questionnaire1.documentTemplateId
    , formatUuid = questionnaire1.formatUuid
    }

questionnaire1EditedChange :: QuestionnaireChangeDTO
questionnaire1EditedChange =
  QuestionnaireChangeDTO
    { name = questionnaire1Edited.name
    , description = questionnaire1Edited.description
    , visibility = questionnaire1Edited.visibility
    , sharing = questionnaire1Edited.sharing
    , projectTags = questionnaire1Edited.projectTags
    , permissions = questionnaire1Edited.permissions
    , documentTemplateId = Nothing
    , formatUuid = Nothing
    , isTemplate = questionnaire1Edited.isTemplate
    }

qtn1AlbertEditPermRecord :: QuestionnairePermRecord
qtn1AlbertEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "d41f9211-e318-4cd6-8b44-36a04f4e30bf"
    , questionnaireUuid = questionnaire1.uuid
    , member = albertMember
    , perms = ownerPermissions
    }

qtn1AlbertEditPermRecordDto :: QuestionnairePermRecordDTO
qtn1AlbertEditPermRecordDto = toUserPermRecordDTO qtn1AlbertEditPermRecord userAlbert

questionnaire1Simple :: QuestionnaireSimple
questionnaire1Simple = toSimple questionnaire1

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire2 :: Questionnaire
questionnaire2 =
  Questionnaire
    { uuid = fromJust (U.fromString "d57520b4-5a70-4d40-8623-af2bfbbdfdfe")
    , name = "My VisibleView Questionnaire"
    , description = Just "Some description"
    , visibility = VisibleViewQuestionnaire
    , sharing = RestrictedQuestionnaire
    , packageId = germanyPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = [_QUESTIONNAIRE_PROJECT_TAG_1, _QUESTIONNAIRE_PROJECT_TAG_2]
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , permissions = [qtn2AlbertEditPermRecord]
    , events = fEvents
    , versions = qVersions
    , isTemplate = False
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    }

questionnaire2Edited :: Questionnaire
questionnaire2Edited =
  Questionnaire
    { uuid = questionnaire2.uuid
    , name = "EDITED: " ++ questionnaire2.name
    , description = Just "EDITED: Some description"
    , visibility = VisibleEditQuestionnaire
    , sharing = RestrictedQuestionnaire
    , packageId = questionnaire2.packageId
    , selectedQuestionTagUuids = questionnaire2.selectedQuestionTagUuids
    , projectTags = questionnaire2.projectTags
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , permissions = []
    , events = questionnaire2.events
    , versions = questionnaire2.versions
    , isTemplate = False
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
    , tenantUuid = defaultTenant.uuid
    , createdAt = questionnaire2.createdAt
    , updatedAt = questionnaire2.updatedAt
    }

questionnaire2Ctn :: QuestionnaireContent
questionnaire2Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

questionnaire2ContentEdited :: Questionnaire
questionnaire2ContentEdited = questionnaire2 {events = fEventsEdited}

questionnaire2Dto :: QuestionnaireDTO
questionnaire2Dto = toSimpleDTO questionnaire2 germanyPackage QSDefault [qtn2AlbertEditPermRecordDto]

qtn2AlbertEditPermRecord :: QuestionnairePermRecord
qtn2AlbertEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "e3989bdc-d176-408a-afb9-b9259ceb0236"
    , questionnaireUuid = questionnaire2.uuid
    , member = albertMember
    , perms = ownerPermissions
    }

qtn2AlbertEditPermRecordDto :: QuestionnairePermRecordDTO
qtn2AlbertEditPermRecordDto = toUserPermRecordDTO qtn2AlbertEditPermRecord userAlbert

questionnaire2Simple :: QuestionnaireSimple
questionnaire2Simple = toSimple questionnaire2

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire3 :: Questionnaire
questionnaire3 =
  Questionnaire
    { uuid = fromJust (U.fromString "16530a07-e673-4ff3-ac1f-57250f2c1bfe")
    , name = "My VisibleEdit Questionnaire"
    , description = Just "Some description"
    , visibility = VisibleEditQuestionnaire
    , sharing = RestrictedQuestionnaire
    , packageId = germanyPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Nothing
    , permissions = []
    , events = fEvents
    , versions = qVersions
    , isTemplate = False
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 28) 0
    }

questionnaire3Ctn :: QuestionnaireContent
questionnaire3Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

questionnaire3ContentEdited :: Questionnaire
questionnaire3ContentEdited = questionnaire1 {events = fEventsEdited}

questionnaire3Dto :: QuestionnaireDTO
questionnaire3Dto = toSimpleDTO questionnaire3 germanyPackage QSDefault []

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire4 :: Questionnaire
questionnaire4 =
  Questionnaire
    { uuid = fromJust (U.fromString "57250a07-a663-4ff3-ac1f-16530f2c1bfe")
    , name = "Outdated Questionnaire"
    , description = Just "Some description"
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , packageId = netherlandsPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Nothing
    , permissions = []
    , events = [sphse_2']
    , versions = []
    , isTemplate = False
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
    , tenantUuid = defaultTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire4Ctn :: QuestionnaireContent
questionnaire4Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase2.uuid
    , replies = M.empty
    , labels = M.empty
    }

questionnaire4VisibleView :: Questionnaire
questionnaire4VisibleView = questionnaire4 {visibility = VisibleViewQuestionnaire}

questionnaire4VisibleEdit :: Questionnaire
questionnaire4VisibleEdit = questionnaire4 {visibility = VisibleEditQuestionnaire}

questionnaire4Upgraded :: Questionnaire
questionnaire4Upgraded =
  questionnaire4
    { uuid = fromJust (U.fromString "5deabef8-f526-421c-90e2-dd7aed1a25c5")
    , packageId = netherlandsPackageV2.pId
    }

questionnaire4VisibleViewUpgraded :: Questionnaire
questionnaire4VisibleViewUpgraded = questionnaire4Upgraded {visibility = VisibleViewQuestionnaire}

questionnaire4VisibleEditUpgraded :: Questionnaire
questionnaire4VisibleEditUpgraded = questionnaire4Upgraded {visibility = VisibleEditQuestionnaire}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire5 :: Questionnaire
questionnaire5 =
  questionnaire1
    { uuid = fromJust (U.fromString "506be867-ba92-4e10-8175-187e99613366")
    , name = "My Private Questionnaire SharedView"
    , visibility = PrivateQuestionnaire
    , sharing = AnyoneWithLinkViewQuestionnaire
    , permissions = [qtn5AlbertEditPermRecord]
    }

questionnaire5ContentEdited :: Questionnaire
questionnaire5ContentEdited = questionnaire5 {events = fEventsEdited}

qtn5AlbertEditPermRecord :: QuestionnairePermRecord
qtn5AlbertEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "0ceedc3e-0165-4e7d-ae7c-ac233b7284de"
    , questionnaireUuid = questionnaire5.uuid
    , member = albertMember
    , perms = ownerPermissions
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire6 :: Questionnaire
questionnaire6 =
  questionnaire1
    { uuid = fromJust (U.fromString "09304abd-2035-4046-8dc8-b3e5ba8c016c")
    , name = "My Private Questionnaire SharedEdit"
    , visibility = PrivateQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    , permissions = [qtn6AlbertEditPermRecord]
    }

questionnaire6Ctn :: QuestionnaireContent
questionnaire6Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

questionnaire6ContentEdited :: Questionnaire
questionnaire6ContentEdited = questionnaire6 {events = fEventsEdited}

questionnaire6Dto :: QuestionnaireDTO
questionnaire6Dto = toSimpleDTO questionnaire6 germanyPackage QSDefault [qtn6AlbertEditPermRecordDto]

qtn6AlbertEditPermRecord :: QuestionnairePermRecord
qtn6AlbertEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "305bf5d7-75a1-4dc4-85af-c092f5010e82"
    , questionnaireUuid = questionnaire6.uuid
    , member = albertMember
    , perms = ownerPermissions
    }

qtn6AlbertEditPermRecordDto :: QuestionnairePermRecordDTO
qtn6AlbertEditPermRecordDto = toUserPermRecordDTO qtn6AlbertEditPermRecord userAlbert

questionnaire6Simple :: QuestionnaireSimple
questionnaire6Simple = toSimple questionnaire6

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire7 :: Questionnaire
questionnaire7 =
  questionnaire2
    { uuid = fromJust (U.fromString "abd22b10-63fd-4cb8-bb23-7997ff32eccc")
    , name = "My VisibleView Questionnaire SharedView"
    , visibility = VisibleViewQuestionnaire
    , sharing = AnyoneWithLinkViewQuestionnaire
    , permissions = [qtn7AlbertEditPermRecord]
    }

questionnaire7ContentEdited :: Questionnaire
questionnaire7ContentEdited = questionnaire7 {events = fEventsEdited}

questionnaire7Ctn :: QuestionnaireContent
questionnaire7Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

qtn7AlbertEditPermRecord :: QuestionnairePermRecord
qtn7AlbertEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "6698fb11-9aac-49a8-867a-a038cff3dded"
    , questionnaireUuid = questionnaire7.uuid
    , member = albertMember
    , perms = ownerPermissions
    }

qtn7AlbertEditPermRecordDto :: QuestionnairePermRecordDTO
qtn7AlbertEditPermRecordDto = toUserPermRecordDTO qtn7AlbertEditPermRecord userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire8 :: Questionnaire
questionnaire8 =
  questionnaire2
    { uuid = fromJust (U.fromString "a990f62a-ca1f-4517-82d4-399951b8630b")
    , name = "My VisibleView Questionnaire SharedEdit"
    , visibility = VisibleViewQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    , permissions = [qtn8AlbertEditPermRecord]
    }

questionnaire8ContentEdited :: Questionnaire
questionnaire8ContentEdited = questionnaire8 {events = fEventsEdited}

qtn8AlbertEditPermRecord :: QuestionnairePermRecord
qtn8AlbertEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "ce6b59d0-c551-4d8e-af0e-40d633238f1d"
    , questionnaireUuid = questionnaire8.uuid
    , member = albertMember
    , perms = ownerPermissions
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire9 :: Questionnaire
questionnaire9 =
  questionnaire2
    { uuid = fromJust (U.fromString "936e852f-4c41-4524-8387-bd87090e9fcc")
    , name = "My VisibleEdit Questionnaire SharedView"
    , visibility = VisibleEditQuestionnaire
    , sharing = AnyoneWithLinkViewQuestionnaire
    , permissions = [qtn9AlbertEditPermRecord]
    }

questionnaire9ContentEdited :: Questionnaire
questionnaire9ContentEdited = questionnaire9 {events = fEventsEdited}

qtn9AlbertEditPermRecord :: QuestionnairePermRecord
qtn9AlbertEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "beb45ac6-a0f4-425b-9d88-fd62ec39baa9"
    , questionnaireUuid = questionnaire9.uuid
    , member = albertMember
    , perms = ownerPermissions
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire10 :: Questionnaire
questionnaire10 =
  questionnaire3
    { uuid = fromJust (U.fromString "3c8e7ce6-cb5e-4cd1-a4d1-fb9de55f67ed")
    , name = "My VisibleEdit Questionnaire SharedEdit"
    , visibility = VisibleEditQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    }

questionnaire10ContentEdited :: Questionnaire
questionnaire10ContentEdited = questionnaire10 {events = fEvents ++ [setCreatedBy slble_rQ2' Nothing]}

questionnaire10Ctn :: QuestionnaireContent
questionnaire10Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

questionnaire10Edited :: Questionnaire
questionnaire10Edited = questionnaire10 {permissions = [qtn10NikolaEditPermRecord]}

qtn10NikolaEditPermRecord :: QuestionnairePermRecord
qtn10NikolaEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "93c0e6c0-5aa0-4feb-b5cd-e54f60bdadbf"
    , questionnaireUuid = questionnaire10.uuid
    , member = nikolaMember
    , perms = ownerPermissions
    }

qtn10NikolaEditPermRecordDto :: QuestionnairePermRecordDTO
qtn10NikolaEditPermRecordDto = toUserPermRecordDTO qtn10NikolaEditPermRecord userNikola

questionnaire10EditedChange :: QuestionnaireChangeDTO
questionnaire10EditedChange =
  QuestionnaireChangeDTO
    { name = "EDITED: " ++ questionnaire10.name
    , description = questionnaire10.description
    , visibility = questionnaire10.visibility
    , sharing = questionnaire10.sharing
    , projectTags = questionnaire10.projectTags
    , permissions = questionnaire10.permissions
    , documentTemplateId = Nothing
    , formatUuid = Nothing
    , isTemplate = questionnaire10.isTemplate
    }

questionnaire10EditedWs :: QuestionnaireDetailWsDTO
questionnaire10EditedWs =
  QuestionnaireDetailWsDTO
    { name = questionnaire10EditedChange.name
    , description = questionnaire10EditedChange.description
    , visibility = questionnaire10EditedChange.visibility
    , sharing = questionnaire10EditedChange.sharing
    , projectTags = questionnaire10EditedChange.projectTags
    , permissions = []
    , documentTemplateId = Nothing
    , documentTemplate = Nothing
    , formatUuid = Nothing
    , format = Nothing
    , isTemplate = questionnaire10EditedChange.isTemplate
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire11 :: Questionnaire
questionnaire11 =
  questionnaire1
    { uuid = fromJust (U.fromString "ba6b6c0e-2bb7-40e7-9019-feb943756888")
    , name = "My Questionnaire from project template"
    , permissions = [qtn11AlbertEditPermRecord]
    }

questionnaire11Ctn :: QuestionnaireContent
questionnaire11Ctn = questionnaire1Ctn

questionnaire11Dto :: QuestionnaireDTO
questionnaire11Dto = toSimpleDTO questionnaire11 germanyPackage QSDefault [qtn11AlbertEditPermRecordDto]

qtn11AlbertEditPermRecord :: QuestionnairePermRecord
qtn11AlbertEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "74e5f2ac-8497-4744-96e2-66325f46d8d2"
    , questionnaireUuid = questionnaire11.uuid
    , member = albertMember
    , perms = ownerPermissions
    }

qtn11AlbertEditPermRecordDto :: QuestionnairePermRecordDTO
qtn11AlbertEditPermRecordDto = toUserPermRecordDTO qtn11AlbertEditPermRecord userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire12 :: Questionnaire
questionnaire12 =
  questionnaire1
    { uuid = u' "e02bc040-7446-48a2-b557-678e01d66937"
    , name = "My Private Questionnaire with 2 users"
    , visibility = VisibleEditQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    , permissions = [qtn12AlbertEditPermRecord, qtn12NikolaEditPermRecord]
    }

questionnaire12Ctn :: QuestionnaireContent
questionnaire12Ctn = questionnaire1Ctn

questionnaire12Dto :: QuestionnaireDTO
questionnaire12Dto =
  toSimpleDTO questionnaire12 germanyPackage QSDefault [qtn12AlbertEditPermRecordDto, qtn12NikolaEditPermRecordDto]

qtn12AlbertEditPermRecord :: QuestionnairePermRecord
qtn12AlbertEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "3c268c5a-6778-447c-bf66-4576470eedaf"
    , questionnaireUuid = questionnaire12.uuid
    , member = albertMember
    , perms = ownerPermissions
    }

qtn12AlbertEditPermRecordDto :: QuestionnairePermRecordDTO
qtn12AlbertEditPermRecordDto = toUserPermRecordDTO qtn12AlbertEditPermRecord userAlbert

qtn12NikolaEditPermRecord :: QuestionnairePermRecord
qtn12NikolaEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "6e3b9e11-f0c8-41ce-aece-5115ddfb63b8"
    , questionnaireUuid = questionnaire12.uuid
    , member = nikolaMember
    , perms = ownerPermissions
    }

qtn12NikolaEditPermRecordDto :: QuestionnairePermRecordDTO
qtn12NikolaEditPermRecordDto = toUserPermRecordDTO qtn12NikolaEditPermRecord userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire13 :: Questionnaire
questionnaire13 =
  questionnaire1
    { uuid = u' "59b97a8e-aa48-47f7-93a7-646f9df077df"
    , name = "My VisibleCommentQuestionnaire Questionnaire"
    , visibility = VisibleCommentQuestionnaire
    , permissions = [qtn13NikolaCommentPermRecord]
    }

questionnaire13Ctn :: QuestionnaireContent
questionnaire13Ctn = questionnaire1Ctn

questionnaire13Dto :: QuestionnaireDTO
questionnaire13Dto = toSimpleDTO questionnaire13 germanyPackage QSDefault [qtn13NikolaCommentPermRecordDto]

qtn13NikolaCommentPermRecord :: QuestionnairePermRecord
qtn13NikolaCommentPermRecord =
  QuestionnairePermRecord
    { uuid = u' "b576befb-0c89-41da-a297-d174133362a2"
    , questionnaireUuid = questionnaire13.uuid
    , member = nikolaMember
    , perms = commentatorPermissions
    }

qtn13NikolaCommentPermRecordDto :: QuestionnairePermRecordDTO
qtn13NikolaCommentPermRecordDto = toUserPermRecordDTO qtn13NikolaCommentPermRecord userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire14 :: Questionnaire
questionnaire14 =
  questionnaire1
    { uuid = u' "8355fe3c-47b9-4078-b5b6-08aa0188e85f"
    , name = "My different KM Questionnaire"
    , permissions = [qtn14NikolaEditPermRecord]
    , packageId = amsterdamPackage.pId
    , events = []
    , answeredQuestions = 0
    , unansweredQuestions = 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 26) 0
    }

questionnaire14Ctn :: QuestionnaireContent
questionnaire14Ctn = questionnaire1Ctn

questionnaire14Dto :: QuestionnaireDTO
questionnaire14Dto = toSimpleDTO questionnaire14 amsterdamPackage QSDefault [qtn14NikolaEditPermRecordDto]

qtn14NikolaEditPermRecord :: QuestionnairePermRecord
qtn14NikolaEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "2508a8fd-016f-48b5-afd4-6d1694ae8192"
    , questionnaireUuid = questionnaire14.uuid
    , member = nikolaMember
    , perms = ownerPermissions
    }

qtn14NikolaEditPermRecordDto :: QuestionnairePermRecordDTO
qtn14NikolaEditPermRecordDto = toUserPermRecordDTO qtn14NikolaEditPermRecord userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
differentQuestionnaire :: Questionnaire
differentQuestionnaire =
  Questionnaire
    { uuid = fromJust (U.fromString "7bf4a83e-1687-4e99-b1df-9221977d7b4f")
    , name = "My Different Questionnaire"
    , description = Just "Some description"
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , packageId = differentPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ anotherWizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userCharles.uuid
    , permissions = [differentQtnCharlesOwnerPermRecord]
    , events = []
    , versions = []
    , isTemplate = True
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
    , tenantUuid = differentTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentQtnCharlesOwnerPermRecord :: QuestionnairePermRecord
differentQtnCharlesOwnerPermRecord =
  QuestionnairePermRecord
    { uuid = u' "e3476103-428e-4729-8f73-adabaad7ef8c"
    , questionnaireUuid = differentQuestionnaire.uuid
    , member = charlesMember
    , perms = ownerPermissions
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
contentChangeDTO :: QuestionnaireContentChangeDTO
contentChangeDTO =
  QuestionnaireContentChangeDTO
    { events = fmap (`toEventChangeDTO` samplePhasesAnsweredIndication) fEvents
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
bioGroupEditPermRecord :: QuestionnairePermRecord
bioGroupEditPermRecord =
  QuestionnairePermRecord
    { uuid = u' "637d5a26-20c8-4704-8cee-c3cacff40ea4"
    , questionnaireUuid = questionnaire1.uuid
    , member = bioGroupMember
    , perms = ownerPermissions
    }

bioGroupEditPermRecordDto :: QuestionnairePermRecordDTO
bioGroupEditPermRecordDto = toGroupPermRecordDTO bioGroupEditPermRecord bioGroup
