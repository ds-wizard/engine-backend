module Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires where

import qualified Data.Map.Strict as M

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
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
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnairePerm
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
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.User.UserGroup

_QUESTIONNAIRE_PROJECT_TAG_1 = "projectTag1"

_QUESTIONNAIRE_PROJECT_TAG_2 = "projectTag2"

questionnaire1 :: Questionnaire
questionnaire1 =
  Questionnaire
    { uuid = u' "af984a75-56e3-49f8-b16f-d6b99599910a"
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
    , permissions = [qtn1AlbertEditQtnPerm]
    , events = fEvents
    , versions = qVersions
    , isTemplate = True
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 25
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
questionnaire1Dto = toSimpleDTO questionnaire1 germanyPackage QSDefault [qtn1AlbertEditQtnPermDto]

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
    , permissions = fmap toQuestionnairePermChangeDTO questionnaire1Edited.permissions
    , documentTemplateId = Nothing
    , formatUuid = Nothing
    , isTemplate = questionnaire1Edited.isTemplate
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

questionnaire1Simple :: QuestionnaireSimple
questionnaire1Simple = toSimple questionnaire1

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire2 :: Questionnaire
questionnaire2 =
  Questionnaire
    { uuid = u' "d57520b4-5a70-4d40-8623-af2bfbbdfdfe"
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
    , permissions = [qtn2AlbertEditQtnPerm]
    , events = fEvents
    , versions = qVersions
    , isTemplate = False
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
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
questionnaire2Dto = toSimpleDTO questionnaire2 germanyPackage QSDefault [qtn2AlbertEditQtnPermDto]

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

questionnaire2Simple :: QuestionnaireSimple
questionnaire2Simple = toSimple questionnaire2

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire3 :: Questionnaire
questionnaire3 =
  Questionnaire
    { uuid = u' "16530a07-e673-4ff3-ac1f-57250f2c1bfe"
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
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 28
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
    { uuid = u' "57250a07-a663-4ff3-ac1f-16530f2c1bfe"
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
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 25
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
    { uuid = u' "5deabef8-f526-421c-90e2-dd7aed1a25c5"
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
    { uuid = u' "506be867-ba92-4e10-8175-187e99613366"
    , name = "My Private Questionnaire SharedView"
    , visibility = PrivateQuestionnaire
    , sharing = AnyoneWithLinkViewQuestionnaire
    , permissions = [qtn5AlbertEditQtnPerm]
    }

questionnaire5ContentEdited :: Questionnaire
questionnaire5ContentEdited = questionnaire5 {events = fEventsEdited}

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
questionnaire6 :: Questionnaire
questionnaire6 =
  questionnaire1
    { uuid = u' "09304abd-2035-4046-8dc8-b3e5ba8c016c"
    , name = "My Private Questionnaire SharedEdit"
    , visibility = PrivateQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    , permissions = [qtn6AlbertEditQtnPerm]
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
questionnaire6Dto = toSimpleDTO questionnaire6 germanyPackage QSDefault [qtn6AlbertEditQtnPermDto]

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

questionnaire6Simple :: QuestionnaireSimple
questionnaire6Simple = toSimple questionnaire6

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire7 :: Questionnaire
questionnaire7 =
  questionnaire2
    { uuid = u' "abd22b10-63fd-4cb8-bb23-7997ff32eccc"
    , name = "My VisibleView Questionnaire SharedView"
    , visibility = VisibleViewQuestionnaire
    , sharing = AnyoneWithLinkViewQuestionnaire
    , permissions = [qtn7AlbertEditQtnPerm]
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
questionnaire8 :: Questionnaire
questionnaire8 =
  questionnaire2
    { uuid = u' "a990f62a-ca1f-4517-82d4-399951b8630b"
    , name = "My VisibleView Questionnaire SharedEdit"
    , visibility = VisibleViewQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    , permissions = [qtn8AlbertEditQtnPerm]
    }

questionnaire8ContentEdited :: Questionnaire
questionnaire8ContentEdited = questionnaire8 {events = fEventsEdited}

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
questionnaire9 :: Questionnaire
questionnaire9 =
  questionnaire2
    { uuid = u' "936e852f-4c41-4524-8387-bd87090e9fcc"
    , name = "My VisibleEdit Questionnaire SharedView"
    , visibility = VisibleEditQuestionnaire
    , sharing = AnyoneWithLinkViewQuestionnaire
    , permissions = [qtn9AlbertEditQtnPerm]
    }

questionnaire9ContentEdited :: Questionnaire
questionnaire9ContentEdited = questionnaire9 {events = fEventsEdited}

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
questionnaire10 :: Questionnaire
questionnaire10 =
  questionnaire3
    { uuid = u' "3c8e7ce6-cb5e-4cd1-a4d1-fb9de55f67ed"
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
questionnaire10Edited = questionnaire10 {permissions = [qtn10NikolaEditQtnPerm]}

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

questionnaire10EditedChange :: QuestionnaireChangeDTO
questionnaire10EditedChange =
  QuestionnaireChangeDTO
    { name = "EDITED: " ++ questionnaire10.name
    , description = questionnaire10.description
    , visibility = questionnaire10.visibility
    , sharing = questionnaire10.sharing
    , projectTags = questionnaire10.projectTags
    , permissions = fmap toQuestionnairePermChangeDTO questionnaire10.permissions
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
    { uuid = u' "ba6b6c0e-2bb7-40e7-9019-feb943756888"
    , name = "My Questionnaire from project template"
    , permissions = [qtn11AlbertEditQtnPerm]
    }

questionnaire11Ctn :: QuestionnaireContent
questionnaire11Ctn = questionnaire1Ctn

questionnaire11Dto :: QuestionnaireDTO
questionnaire11Dto = toSimpleDTO questionnaire11 germanyPackage QSDefault [qtn11AlbertEditQtnPermDto]

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
questionnaire12 :: Questionnaire
questionnaire12 =
  questionnaire1
    { uuid = u' "e02bc040-7446-48a2-b557-678e01d66937"
    , name = "My Private Questionnaire with 2 users"
    , visibility = VisibleEditQuestionnaire
    , sharing = AnyoneWithLinkEditQuestionnaire
    , permissions = [qtn12NikolaEditQtnPerm, qtn12AlbertEditQtnPerm]
    }

questionnaire12Ctn :: QuestionnaireContent
questionnaire12Ctn = questionnaire1Ctn

questionnaire12Dto :: QuestionnaireDTO
questionnaire12Dto =
  toSimpleDTO questionnaire12 germanyPackage QSDefault [qtn12NikolaEditQtnPermDto, qtn12AlbertEditQtnPermDto]

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
questionnaire13 :: Questionnaire
questionnaire13 =
  questionnaire1
    { uuid = u' "59b97a8e-aa48-47f7-93a7-646f9df077df"
    , name = "My VisibleCommentQuestionnaire Questionnaire"
    , visibility = VisibleCommentQuestionnaire
    , permissions = [qtn13NikolaCommentQtnPerm]
    }

questionnaire13Ctn :: QuestionnaireContent
questionnaire13Ctn = questionnaire1Ctn

questionnaire13Dto :: QuestionnaireDTO
questionnaire13Dto = toSimpleDTO questionnaire13 germanyPackage QSDefault [qtn13NikolaCommentQtnPermDto]

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
    , packageId = amsterdamPackage.pId
    , events = []
    , answeredQuestions = 0
    , unansweredQuestions = 0
    , updatedAt = dt' 2018 1 26
    }

questionnaire14Ctn :: QuestionnaireContent
questionnaire14Ctn = questionnaire1Ctn

questionnaire14Dto :: QuestionnaireDTO
questionnaire14Dto = toSimpleDTO questionnaire14 amsterdamPackage QSDefault [qtn14NikolaEditQtnPermDto]

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
questionnaire15 :: Questionnaire
questionnaire15 =
  Questionnaire
    { uuid = u' "d09695f4-638b-472b-9951-a31bd7dc91f7"
    , name = "My Group Questionnaire"
    , description = Just "Some description"
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , packageId = germanyPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just wizardDocumentTemplate.tId
    , formatUuid = Just formatJson.uuid
    , creatorUuid = Nothing
    , permissions = [qtn15GroupEditQtnPerm]
    , events = fEvents
    , versions = qVersions
    , isTemplate = False
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 29
    }

questionnaire15Dto :: QuestionnaireDTO
questionnaire15Dto = toSimpleDTO questionnaire15 germanyPackage QSDefault [qtn15GroupEditQtnPermDto]

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
    , packageId = differentPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ anotherWizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userCharles.uuid
    , permissions = [differentCharlesOwnerQtnPerm]
    , events = []
    , versions = []
    , isTemplate = True
    , squashed = True
    , answeredQuestions = 3
    , unansweredQuestions = 1
    , tenantUuid = differentTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 25
    }

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
    { events = fmap (`toEventChangeDTO` samplePhasesAnsweredIndication) fEvents
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
