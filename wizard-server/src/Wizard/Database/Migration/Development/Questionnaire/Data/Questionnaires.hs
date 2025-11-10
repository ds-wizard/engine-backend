module Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.QuestionnaireMapper
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.User.UserGroup

_QUESTIONNAIRE_PROJECT_TAG_1 = "projectTag1"

_QUESTIONNAIRE_PROJECT_TAG_2 = "projectTag2"

questionnaire1Uuid :: U.UUID
questionnaire1Uuid = u' "af984a75-56e3-49f8-b16f-d6b99599910a"

questionnaire1 :: Questionnaire
questionnaire1 =
  Questionnaire
    { uuid = questionnaire1Uuid
    , name = "My Private Questionnaire"
    , description = Just "Some description"
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , knowledgeModelPackageId = germanyKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = [_QUESTIONNAIRE_PROJECT_TAG_1]
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , permissions = [qtn1AlbertEditQtnPerm]
    , isTemplate = True
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 25
    }

questionnaire1Events :: [QuestionnaireEvent]
questionnaire1Events = fEvents questionnaire1Uuid

questionnaire1Edited :: Questionnaire
questionnaire1Edited =
  questionnaire1
    { name = "EDITED: " ++ questionnaire1.name
    , visibility = VisibleEditQuestionnaire
    , sharing = RestrictedQuestionnaire
    , projectTags = [_QUESTIONNAIRE_PROJECT_TAG_1, _QUESTIONNAIRE_PROJECT_TAG_2]
    , permissions = []
    }

questionnaire1ShareEdited :: Questionnaire
questionnaire1ShareEdited =
  questionnaire1
    { visibility = VisibleEditQuestionnaire
    , sharing = RestrictedQuestionnaire
    , permissions = []
    }

questionnaire1SettingsEdited :: Questionnaire
questionnaire1SettingsEdited =
  questionnaire1
    { name = "EDITED: " ++ questionnaire1.name
    , projectTags = [_QUESTIONNAIRE_PROJECT_TAG_1, _QUESTIONNAIRE_PROJECT_TAG_2]
    }

questionnaire1EventsEdited :: [QuestionnaireEvent]
questionnaire1EventsEdited = fEventsEdited questionnaire1Uuid

questionnaire1Versions :: [QuestionnaireVersion]
questionnaire1Versions = qVersions questionnaire1Uuid

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
    , events = [toEventDTO (sre_rQ1' questionnaire1Uuid) (Just userAlbert), toEventDTO (sre_rQ2' questionnaire1Uuid) (Just userAlbert)]
    , versions = []
    }

questionnaire1Dto :: QuestionnaireDTO
questionnaire1Dto = toSimpleDTO questionnaire1 germanyKmPackage QSDefault [qtn1AlbertEditQtnPermDto]

questionnaire1Create :: QuestionnaireCreateDTO
questionnaire1Create =
  QuestionnaireCreateDTO
    { name = questionnaire1.name
    , knowledgeModelPackageId = questionnaire1.knowledgeModelPackageId
    , visibility = questionnaire1.visibility
    , sharing = questionnaire1.sharing
    , questionTagUuids = []
    , documentTemplateId = questionnaire1.documentTemplateId
    , formatUuid = questionnaire1.formatUuid
    }

questionnaire1EditedShareChange :: QuestionnaireShareChangeDTO
questionnaire1EditedShareChange =
  QuestionnaireShareChangeDTO
    { visibility = questionnaire1ShareEdited.visibility
    , sharing = questionnaire1ShareEdited.sharing
    , permissions = fmap toQuestionnairePermChangeDTO questionnaire1ShareEdited.permissions
    }

questionnaire1SettingsChange :: QuestionnaireSettingsChangeDTO
questionnaire1SettingsChange =
  QuestionnaireSettingsChangeDTO
    { name = questionnaire1SettingsEdited.name
    , description = questionnaire1SettingsEdited.description
    , projectTags = questionnaire1SettingsEdited.projectTags
    , documentTemplateId = Nothing
    , formatUuid = Nothing
    , isTemplate = questionnaire1SettingsEdited.isTemplate
    }

qtn1AlbertEditQtnPerm :: QuestionnairePerm
qtn1AlbertEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire1.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn1AlbertEditQtnPermDto :: QuestionnairePermDTO
qtn1AlbertEditQtnPermDto = toUserQuestionnairePermDTO qtn1AlbertEditQtnPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire2Uuid :: U.UUID
questionnaire2Uuid = u' "d57520b4-5a70-4d40-8623-af2bfbbdfdfe"

questionnaire2 :: Questionnaire
questionnaire2 =
  Questionnaire
    { uuid = questionnaire2Uuid
    , name = "My VisibleView Questionnaire"
    , description = Just "Some description"
    , visibility = VisibleViewQuestionnaire
    , sharing = RestrictedQuestionnaire
    , knowledgeModelPackageId = germanyKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = [_QUESTIONNAIRE_PROJECT_TAG_1, _QUESTIONNAIRE_PROJECT_TAG_2]
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , permissions = [qtn2AlbertEditQtnPerm]
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 22
    }

questionnaire2Edited :: Questionnaire
questionnaire2Edited =
  Questionnaire
    { uuid = questionnaire2.uuid
    , name = "EDITED: " ++ questionnaire2.name
    , description = Just "EDITED: Some description"
    , visibility = VisibleEditQuestionnaire
    , sharing = RestrictedQuestionnaire
    , knowledgeModelPackageId = questionnaire2.knowledgeModelPackageId
    , selectedQuestionTagUuids = questionnaire2.selectedQuestionTagUuids
    , projectTags = questionnaire2.projectTags
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , permissions = []
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = questionnaire2.createdAt
    , updatedAt = questionnaire2.updatedAt
    }

questionnaire2Events :: [QuestionnaireEvent]
questionnaire2Events = fEvents questionnaire2Uuid

questionnaire2Versions :: [QuestionnaireVersion]
questionnaire2Versions = qVersions questionnaire2Uuid

questionnaire2ShareEdited :: Questionnaire
questionnaire2ShareEdited =
  questionnaire2
    { visibility = VisibleEditQuestionnaire
    , sharing = RestrictedQuestionnaire
    , permissions = []
    }

questionnaire2SettingsEdited :: Questionnaire
questionnaire2SettingsEdited =
  questionnaire2
    { name = "EDITED: " ++ questionnaire2.name
    , description = Just "EDITED: Some description"
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , isTemplate = False
    }

questionnaire2Ctn :: QuestionnaireContent
questionnaire2Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

questionnaire2EventsEdited :: [QuestionnaireEvent]
questionnaire2EventsEdited = fEventsEdited questionnaire2Uuid

questionnaire2Dto :: QuestionnaireDTO
questionnaire2Dto = toSimpleDTO questionnaire2 germanyKmPackage QSDefault [qtn2AlbertEditQtnPermDto]

qtn2AlbertEditQtnPerm :: QuestionnairePerm
qtn2AlbertEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire2.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn2AlbertEditQtnPermDto :: QuestionnairePermDTO
qtn2AlbertEditQtnPermDto = toUserQuestionnairePermDTO qtn2AlbertEditQtnPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire3Uuid :: U.UUID
questionnaire3Uuid = u' "16530a07-e673-4ff3-ac1f-57250f2c1bfe"

questionnaire3 :: Questionnaire
questionnaire3 =
  Questionnaire
    { uuid = questionnaire3Uuid
    , name = "My VisibleEdit Questionnaire"
    , description = Just "Some description"
    , visibility = VisibleEditQuestionnaire
    , sharing = RestrictedQuestionnaire
    , knowledgeModelPackageId = germanyKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Nothing
    , permissions = []
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 28
    }

questionnaire3Events :: [QuestionnaireEvent]
questionnaire3Events = fEvents questionnaire3Uuid

questionnaire3Versions :: [QuestionnaireVersion]
questionnaire3Versions = qVersions questionnaire3Uuid

questionnaire3Ctn :: QuestionnaireContent
questionnaire3Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

questionnaire3EventsEdited :: [QuestionnaireEvent]
questionnaire3EventsEdited = fEventsEdited questionnaire3Uuid

questionnaire3Dto :: QuestionnaireDTO
questionnaire3Dto = toSimpleDTO questionnaire3 germanyKmPackage QSDefault []

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire4Uuid :: U.UUID
questionnaire4Uuid = u' "57250a07-a663-4ff3-ac1f-16530f2c1bfe"

questionnaire4 :: Questionnaire
questionnaire4 =
  Questionnaire
    { uuid = questionnaire4Uuid
    , name = "Outdated Questionnaire"
    , description = Just "Some description"
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , knowledgeModelPackageId = netherlandsKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Nothing
    , permissions = []
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 25
    }

questionnaire4Events :: [QuestionnaireEvent]
questionnaire4Events = [sphse_2' questionnaire4Uuid]

questionnaire4Versions :: [QuestionnaireVersion]
questionnaire4Versions = []

questionnaire4Ctn :: QuestionnaireContent
questionnaire4Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase2.uuid
    , replies = M.empty
    , labels = M.empty
    }

questionnaire4VisibleView :: Questionnaire
questionnaire4VisibleView = questionnaire4 {visibility = VisibleViewQuestionnaire}

questionnaire4VisibleViewEvents :: [QuestionnaireEvent]
questionnaire4VisibleViewEvents = [sphse_2' questionnaire4VisibleView.uuid]

questionnaire4VisibleEdit :: Questionnaire
questionnaire4VisibleEdit = questionnaire4 {visibility = VisibleEditQuestionnaire}

questionnaire4VisibleEditEvents :: [QuestionnaireEvent]
questionnaire4VisibleEditEvents = [sphse_2' questionnaire4VisibleEdit.uuid]

questionnaire4Upgraded :: Questionnaire
questionnaire4Upgraded =
  questionnaire4
    { uuid = u' "5deabef8-f526-421c-90e2-dd7aed1a25c5"
    , knowledgeModelPackageId = netherlandsKmPackageV2.pId
    }

questionnaire4UpgradedEvents :: [QuestionnaireEvent]
questionnaire4UpgradedEvents = [sphse_2' questionnaire4Upgraded.uuid]

questionnaire4VisibleViewUpgraded :: Questionnaire
questionnaire4VisibleViewUpgraded = questionnaire4Upgraded {visibility = VisibleViewQuestionnaire}

questionnaire4VisibleViewUpgradedEvents :: [QuestionnaireEvent]
questionnaire4VisibleViewUpgradedEvents = [sphse_2' questionnaire4VisibleViewUpgraded.uuid]

questionnaire4VisibleEditUpgraded :: Questionnaire
questionnaire4VisibleEditUpgraded = questionnaire4Upgraded {visibility = VisibleEditQuestionnaire}

questionnaire4VisibleEditUpgradedEvents :: [QuestionnaireEvent]
questionnaire4VisibleEditUpgradedEvents = [sphse_2' questionnaire4VisibleEditUpgraded.uuid]

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire5Uuid :: U.UUID
questionnaire5Uuid = u' "506be867-ba92-4e10-8175-187e99613366"

questionnaire5 :: Questionnaire
questionnaire5 =
  questionnaire1
    { uuid = questionnaire5Uuid
    , name = "My Private Questionnaire SharedView"
    , visibility = PrivateQuestionnaire
    , sharing = AnyoneWithLinkViewQuestionnaire
    , permissions = [qtn5AlbertEditQtnPerm]
    }

questionnaire5Events :: [QuestionnaireEvent]
questionnaire5Events = fEvents questionnaire5Uuid

questionnaire5EventsEdited :: [QuestionnaireEvent]
questionnaire5EventsEdited = fEventsEdited questionnaire5Uuid

questionnaire5Versions :: [QuestionnaireVersion]
questionnaire5Versions = qVersions questionnaire5Uuid

qtn5AlbertEditQtnPerm :: QuestionnairePerm
qtn5AlbertEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire5.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire6Uuid :: U.UUID
questionnaire6Uuid = u' "09304abd-2035-4046-8dc8-b3e5ba8c016c"

questionnaire6 :: Questionnaire
questionnaire6 =
  questionnaire1
    { uuid = questionnaire6Uuid
    , name = "My Private Questionnaire SharedEdit"
    , visibility = PrivateQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    , permissions = [qtn6AlbertEditQtnPerm]
    }

questionnaire6Events :: [QuestionnaireEvent]
questionnaire6Events = fEvents questionnaire6Uuid

questionnaire6Versions :: [QuestionnaireVersion]
questionnaire6Versions = qVersions questionnaire6Uuid

questionnaire6Ctn :: QuestionnaireContent
questionnaire6Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

questionnaire6EventsEdited :: [QuestionnaireEvent]
questionnaire6EventsEdited = fEventsEdited questionnaire6Uuid

questionnaire6Dto :: QuestionnaireDTO
questionnaire6Dto = toSimpleDTO questionnaire6 germanyKmPackage QSDefault [qtn6AlbertEditQtnPermDto]

qtn6AlbertEditQtnPerm :: QuestionnairePerm
qtn6AlbertEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire6.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn6AlbertEditQtnPermDto :: QuestionnairePermDTO
qtn6AlbertEditQtnPermDto = toUserQuestionnairePermDTO qtn6AlbertEditQtnPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire7Uuid :: U.UUID
questionnaire7Uuid = u' "abd22b10-63fd-4cb8-bb23-7997ff32eccc"

questionnaire7 :: Questionnaire
questionnaire7 =
  questionnaire2
    { uuid = questionnaire7Uuid
    , name = "My VisibleView Questionnaire SharedView"
    , visibility = VisibleViewQuestionnaire
    , sharing = AnyoneWithLinkViewQuestionnaire
    , permissions = [qtn7AlbertEditQtnPerm]
    }

questionnaire7Events :: [QuestionnaireEvent]
questionnaire7Events = fEvents questionnaire7Uuid

questionnaire7EventsEdited :: [QuestionnaireEvent]
questionnaire7EventsEdited = fEventsEdited questionnaire8Uuid

questionnaire7Versions :: [QuestionnaireVersion]
questionnaire7Versions = qVersions questionnaire7Uuid

questionnaire7Ctn :: QuestionnaireContent
questionnaire7Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

qtn7AlbertEditQtnPerm :: QuestionnairePerm
qtn7AlbertEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire7.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn7AlbertEditQtnPermDto :: QuestionnairePermDTO
qtn7AlbertEditQtnPermDto = toUserQuestionnairePermDTO qtn7AlbertEditQtnPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire8Uuid :: U.UUID
questionnaire8Uuid = u' "a990f62a-ca1f-4517-82d4-399951b8630b"

questionnaire8 :: Questionnaire
questionnaire8 =
  questionnaire2
    { uuid = questionnaire8Uuid
    , name = "My VisibleView Questionnaire SharedEdit"
    , visibility = VisibleViewQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    , permissions = [qtn8AlbertEditQtnPerm]
    }

questionnaire8Events :: [QuestionnaireEvent]
questionnaire8Events = fEvents questionnaire8Uuid

questionnaire8EventsEdited :: [QuestionnaireEvent]
questionnaire8EventsEdited = fEventsEdited questionnaire8Uuid

questionnaire8Versions :: [QuestionnaireVersion]
questionnaire8Versions = qVersions questionnaire8Uuid

qtn8AlbertEditQtnPerm :: QuestionnairePerm
qtn8AlbertEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire8.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire9Uuid :: U.UUID
questionnaire9Uuid = u' "936e852f-4c41-4524-8387-bd87090e9fcc"

questionnaire9 :: Questionnaire
questionnaire9 =
  questionnaire2
    { uuid = questionnaire9Uuid
    , name = "My VisibleEdit Questionnaire SharedView"
    , visibility = VisibleEditQuestionnaire
    , sharing = AnyoneWithLinkViewQuestionnaire
    , permissions = [qtn9AlbertEditQtnPerm]
    }

questionnaire9Events :: [QuestionnaireEvent]
questionnaire9Events = fEvents questionnaire9Uuid

questionnaire9EventsEdited :: [QuestionnaireEvent]
questionnaire9EventsEdited = fEventsEdited questionnaire9Uuid

questionnaire9Versions :: [QuestionnaireVersion]
questionnaire9Versions = qVersions questionnaire9Uuid

qtn9AlbertEditQtnPerm :: QuestionnairePerm
qtn9AlbertEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire9.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire10Uuid :: U.UUID
questionnaire10Uuid = u' "3c8e7ce6-cb5e-4cd1-a4d1-fb9de55f67ed"

questionnaire10 :: Questionnaire
questionnaire10 =
  questionnaire3
    { uuid = questionnaire10Uuid
    , name = "My VisibleEdit Questionnaire SharedEdit"
    , visibility = VisibleEditQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    }

questionnaire10Events :: [QuestionnaireEvent]
questionnaire10Events = fEvents questionnaire10Uuid

questionnaire10EventsEdited :: [QuestionnaireEvent]
questionnaire10EventsEdited = questionnaire10Events ++ [setCreatedBy (slble_rQ2' questionnaire10Uuid) Nothing]

questionnaire10Versions :: [QuestionnaireVersion]
questionnaire10Versions = qVersions questionnaire10Uuid

questionnaire10Ctn :: QuestionnaireContent
questionnaire10Ctn =
  QuestionnaireContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

questionnaire10EditedShare :: Questionnaire
questionnaire10EditedShare = questionnaire10 {permissions = [qtn10NikolaEditQtnPerm]}

questionnaire10EditedSettings :: Questionnaire
questionnaire10EditedSettings = questionnaire10 {name = "EDITED: " ++ questionnaire10.name}

qtn10NikolaEditQtnPerm :: QuestionnairePerm
qtn10NikolaEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire10.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userNikola.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn10NikolaEditQtnPermDto :: QuestionnairePermDTO
qtn10NikolaEditQtnPermDto = toUserQuestionnairePermDTO qtn10NikolaEditQtnPerm userNikola

questionnaire10EditedSettingsChange :: QuestionnaireSettingsChangeDTO
questionnaire10EditedSettingsChange =
  QuestionnaireSettingsChangeDTO
    { name = "EDITED: " ++ questionnaire10.name
    , description = questionnaire10.description
    , projectTags = questionnaire10.projectTags
    , documentTemplateId = Nothing
    , formatUuid = Nothing
    , isTemplate = questionnaire10.isTemplate
    }

questionnaire10EditedWs :: QuestionnaireDetailWsDTO
questionnaire10EditedWs =
  QuestionnaireDetailWsDTO
    { name = questionnaire10EditedSettingsChange.name
    , description = questionnaire10EditedSettingsChange.description
    , visibility = questionnaire10.visibility
    , sharing = questionnaire10.sharing
    , projectTags = questionnaire10EditedSettingsChange.projectTags
    , permissions = []
    , documentTemplateId = Nothing
    , documentTemplate = Nothing
    , formatUuid = Nothing
    , format = Nothing
    , isTemplate = questionnaire10EditedSettingsChange.isTemplate
    , labels = M.empty
    , unresolvedCommentCounts = M.empty
    , resolvedCommentCounts = M.empty
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire11Uuid :: U.UUID
questionnaire11Uuid = u' "ba6b6c0e-2bb7-40e7-9019-feb943756888"

questionnaire11 :: Questionnaire
questionnaire11 =
  questionnaire1
    { uuid = questionnaire11Uuid
    , name = "My Questionnaire from project template"
    , permissions = [qtn11AlbertEditQtnPerm]
    }

questionnaire11Events :: [QuestionnaireEvent]
questionnaire11Events = fEvents questionnaire11Uuid

questionnaire11Versions :: [QuestionnaireVersion]
questionnaire11Versions = qVersions questionnaire11Uuid

questionnaire11Ctn :: QuestionnaireContent
questionnaire11Ctn = questionnaire1Ctn

questionnaire11Dto :: QuestionnaireDTO
questionnaire11Dto = toSimpleDTO questionnaire11 germanyKmPackage QSDefault [qtn11AlbertEditQtnPermDto]

qtn11AlbertEditQtnPerm :: QuestionnairePerm
qtn11AlbertEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire11.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn11AlbertEditQtnPermDto :: QuestionnairePermDTO
qtn11AlbertEditQtnPermDto = toUserQuestionnairePermDTO qtn11AlbertEditQtnPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire12Uuid :: U.UUID
questionnaire12Uuid = u' "e02bc040-7446-48a2-b557-678e01d66937"

questionnaire12 :: Questionnaire
questionnaire12 =
  questionnaire1
    { uuid = questionnaire12Uuid
    , name = "My Private Questionnaire with 2 users"
    , visibility = VisibleEditQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    , permissions = [qtn12NikolaEditQtnPerm, qtn12AlbertEditQtnPerm]
    , updatedAt = dt' 2018 1 23
    }

questionnaire12Events :: [QuestionnaireEvent]
questionnaire12Events = fEvents questionnaire12Uuid

questionnaire12Versions :: [QuestionnaireVersion]
questionnaire12Versions = qVersions questionnaire12Uuid

questionnaire12Ctn :: QuestionnaireContent
questionnaire12Ctn = questionnaire1Ctn

questionnaire12Dto :: QuestionnaireDTO
questionnaire12Dto =
  toSimpleDTO questionnaire12 germanyKmPackage QSDefault [qtn12NikolaEditQtnPermDto, qtn12AlbertEditQtnPermDto]

qtn12AlbertEditQtnPerm :: QuestionnairePerm
qtn12AlbertEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire12.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn12AlbertEditQtnPermDto :: QuestionnairePermDTO
qtn12AlbertEditQtnPermDto = toUserQuestionnairePermDTO qtn12AlbertEditQtnPerm userAlbert

qtn12NikolaEditQtnPerm :: QuestionnairePerm
qtn12NikolaEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire12.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userNikola.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn12NikolaEditQtnPermDto :: QuestionnairePermDTO
qtn12NikolaEditQtnPermDto = toUserQuestionnairePermDTO qtn12NikolaEditQtnPerm userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire13Uuid :: U.UUID
questionnaire13Uuid = u' "59b97a8e-aa48-47f7-93a7-646f9df077df"

questionnaire13 :: Questionnaire
questionnaire13 =
  questionnaire1
    { uuid = questionnaire13Uuid
    , name = "My VisibleCommentQuestionnaire Questionnaire"
    , visibility = VisibleCommentQuestionnaire
    , permissions = [qtn13NikolaCommentQtnPerm]
    }

questionnaire13Events :: [QuestionnaireEvent]
questionnaire13Events = fEvents questionnaire13Uuid

questionnaire13Versions :: [QuestionnaireVersion]
questionnaire13Versions = qVersions questionnaire13Uuid

questionnaire13Ctn :: QuestionnaireContent
questionnaire13Ctn = questionnaire1Ctn

questionnaire13Dto :: QuestionnaireDTO
questionnaire13Dto = toSimpleDTO questionnaire13 germanyKmPackage QSDefault [qtn13NikolaCommentQtnPermDto]

qtn13NikolaCommentQtnPerm :: QuestionnairePerm
qtn13NikolaCommentQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire13.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userNikola.uuid
    , perms = commentatorPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn13NikolaCommentQtnPermDto :: QuestionnairePermDTO
qtn13NikolaCommentQtnPermDto = toUserQuestionnairePermDTO qtn13NikolaCommentQtnPerm userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire14 :: Questionnaire
questionnaire14 =
  questionnaire1
    { uuid = u' "8355fe3c-47b9-4078-b5b6-08aa0188e85f"
    , name = "My different KM Questionnaire"
    , permissions = [qtn14NikolaEditQtnPerm]
    , knowledgeModelPackageId = amsterdamKmPackage.pId
    , updatedAt = dt' 2018 1 26
    }

questionnaire14Events :: [QuestionnaireEvent]
questionnaire14Events = []

questionnaire14Ctn :: QuestionnaireContent
questionnaire14Ctn = questionnaire1Ctn

questionnaire14Dto :: QuestionnaireDTO
questionnaire14Dto = toSimpleDTO questionnaire14 amsterdamKmPackage QSDefault [qtn14NikolaEditQtnPermDto]

qtn14NikolaEditQtnPerm :: QuestionnairePerm
qtn14NikolaEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire14.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userNikola.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn14NikolaEditQtnPermDto :: QuestionnairePermDTO
qtn14NikolaEditQtnPermDto = toUserQuestionnairePermDTO qtn14NikolaEditQtnPerm userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire15Uuid :: U.UUID
questionnaire15Uuid = u' "d09695f4-638b-472b-9951-a31bd7dc91f7"

questionnaire15 :: Questionnaire
questionnaire15 =
  Questionnaire
    { uuid = questionnaire15Uuid
    , name = "My Group Questionnaire"
    , description = Just "Some description"
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , knowledgeModelPackageId = germanyKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just wizardDocumentTemplate.tId
    , formatUuid = Just formatJson.uuid
    , creatorUuid = Nothing
    , permissions = [qtn15GroupEditQtnPerm]
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 29
    }

questionnaire15AnonymousEdit :: Questionnaire
questionnaire15AnonymousEdit =
  questionnaire15
    { sharing = AnyoneWithLinkEditQuestionnaire
    }

questionnaire15AnonymousComment :: Questionnaire
questionnaire15AnonymousComment =
  questionnaire15
    { sharing = AnyoneWithLinkCommentQuestionnaire
    }

questionnaire15NoPerms :: Questionnaire
questionnaire15NoPerms =
  questionnaire15
    { permissions = []
    }

questionnaire15Events :: [QuestionnaireEvent]
questionnaire15Events = fEvents questionnaire15Uuid

questionnaire15Versions :: [QuestionnaireVersion]
questionnaire15Versions = qVersions questionnaire15Uuid

questionnaire15Dto :: QuestionnaireDTO
questionnaire15Dto = toSimpleDTO questionnaire15 germanyKmPackage QSDefault [qtn15GroupEditQtnPermDto]

qtn15GroupEditQtnPerm :: QuestionnairePerm
qtn15GroupEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire15.uuid
    , memberType = UserGroupQuestionnairePermType
    , memberUuid = bioGroup.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn15GroupViewQtnPerm :: QuestionnairePerm
qtn15GroupViewQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire15.uuid
    , memberType = UserGroupQuestionnairePermType
    , memberUuid = animalGroup.uuid
    , perms = viewerPermissions
    , tenantUuid = defaultTenant.uuid
    }

qtn15GroupEditQtnPermDto :: QuestionnairePermDTO
qtn15GroupEditQtnPermDto = toUserGroupQuestionnairePermDTO qtn15GroupEditQtnPerm bioGroup

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
differentQuestionnaire :: Questionnaire
differentQuestionnaire =
  Questionnaire
    { uuid = u' "7bf4a83e-1687-4e99-b1df-9221977d7b4f"
    , name = "My Different Questionnaire"
    , description = Just "Some description"
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , knowledgeModelPackageId = differentPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ anotherWizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userCharles.uuid
    , permissions = [differentCharlesOwnerQtnPerm]
    , isTemplate = True
    , squashed = True
    , tenantUuid = differentTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 25
    }

differentQuestionnaireEvents :: [QuestionnaireEvent]
differentQuestionnaireEvents = []

differentQuestionnaireVersions :: [QuestionnaireVersion]
differentQuestionnaireVersions = []

differentCharlesOwnerQtnPerm :: QuestionnairePerm
differentCharlesOwnerQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = differentQuestionnaire.uuid
    , memberType = UserQuestionnairePermType
    , memberUuid = userCharles.uuid
    , perms = ownerPermissions
    , tenantUuid = differentTenant.uuid
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
contentChangeDTO :: QuestionnaireContentChangeDTO
contentChangeDTO =
  QuestionnaireContentChangeDTO
    { events = fmap toEventChangeDTO (fEvents U.nil)
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
bioGroupEditQtnPerm :: QuestionnairePerm
bioGroupEditQtnPerm =
  QuestionnairePerm
    { questionnaireUuid = questionnaire1.uuid
    , memberType = UserGroupQuestionnairePermType
    , memberUuid = bioGroup.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

bioGroupEditQtnPermDto :: QuestionnairePermDTO
bioGroupEditQtnPermDto = toUserGroupQuestionnairePermDTO bioGroupEditQtnPerm bioGroup
