module Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Common.Lens
import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.Acl.Data.Members
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.QuestionnaireMapper

_QUESTIONNAIRE_PROJECT_TAG_1 = "projectTag1"

_QUESTIONNAIRE_PROJECT_TAG_2 = "projectTag2"

questionnaire1 :: Questionnaire
questionnaire1 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "af984a75-56e3-49f8-b16f-d6b99599910a")
    , _questionnaireName = "My Private Questionnaire"
    , _questionnaireDescription = Just "Some description"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedQuestionTagUuids = []
    , _questionnaireProjectTags = [_QUESTIONNAIRE_PROJECT_TAG_1]
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnairePermissions = [qtn1AlbertEditPermRecord]
    , _questionnaireEvents = fEvents
    , _questionnaireVersions = qVersions
    , _questionnaireIsTemplate = True
    , _questionnaireSquashed = True
    , _questionnaireAnsweredQuestions = 3
    , _questionnaireUnansweredQuestions = 1
    , _questionnaireAppUuid = defaultApp ^. uuid
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire1Edited :: Questionnaire
questionnaire1Edited =
  questionnaire1
    { _questionnaireName = "EDITED: " ++ (questionnaire1 ^. name)
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnaireProjectTags = [_QUESTIONNAIRE_PROJECT_TAG_1, _QUESTIONNAIRE_PROJECT_TAG_2]
    , _questionnairePermissions = []
    }

questionnaire1ContentEdited :: Questionnaire
questionnaire1ContentEdited = questionnaire1 {_questionnaireEvents = fEventsEdited}

questionnaire1Ctn :: QuestionnaireContent
questionnaire1Ctn =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid = Just $ phase1 ^. uuid
    , _questionnaireContentReplies = fReplies
    , _questionnaireContentCommentThreadsMap = qtnThreads
    , _questionnaireContentLabels = fLabels
    }

questionnaire1CtnRevertedDto :: QuestionnaireContentDTO
questionnaire1CtnRevertedDto =
  QuestionnaireContentDTO
    { _questionnaireContentDTOPhaseUuid = Nothing
    , _questionnaireContentDTOReplies = M.fromList [rQ1, rQ2]
    , _questionnaireContentDTOCommentThreadsMap = M.empty
    , _questionnaireContentDTOLabels = M.empty
    , _questionnaireContentDTOEvents = [toEventDTO sre_rQ1' (Just userAlbert), toEventDTO sre_rQ2' (Just userAlbert)]
    , _questionnaireContentDTOVersions = []
    }

questionnaire1Dto :: QuestionnaireDTO
questionnaire1Dto = toSimpleDTO questionnaire1 germanyPackage QSDefault [qtn1AlbertEditPermRecordDto]

questionnaire1Create :: QuestionnaireCreateDTO
questionnaire1Create =
  QuestionnaireCreateDTO
    { _questionnaireCreateDTOName = questionnaire1 ^. name
    , _questionnaireCreateDTOPackageId = questionnaire1 ^. packageId
    , _questionnaireCreateDTOVisibility = questionnaire1 ^. visibility
    , _questionnaireCreateDTOSharing = questionnaire1 ^. sharing
    , _questionnaireCreateDTOQuestionTagUuids = []
    , _questionnaireCreateDTOTemplateId = questionnaire1 ^. templateId
    , _questionnaireCreateDTOFormatUuid = questionnaire1 ^. formatUuid
    }

questionnaire1EditedChange :: QuestionnaireChangeDTO
questionnaire1EditedChange =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName = questionnaire1Edited ^. name
    , _questionnaireChangeDTODescription = questionnaire1Edited ^. description
    , _questionnaireChangeDTOVisibility = questionnaire1Edited ^. visibility
    , _questionnaireChangeDTOSharing = questionnaire1Edited ^. sharing
    , _questionnaireChangeDTOProjectTags = questionnaire1Edited ^. projectTags
    , _questionnaireChangeDTOPermissions = questionnaire1Edited ^. permissions
    , _questionnaireChangeDTOTemplateId = Nothing
    , _questionnaireChangeDTOFormatUuid = Nothing
    , _questionnaireChangeDTOIsTemplate = questionnaire1Edited ^. isTemplate
    }

qtn1AlbertEditPermRecord :: QuestionnairePermRecord
qtn1AlbertEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "d41f9211-e318-4cd6-8b44-36a04f4e30bf"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire1 ^. uuid
    , _questionnairePermRecordMember = albertMember
    , _questionnairePermRecordPerms = ownerPermissions
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
    { _questionnaireUuid = fromJust (U.fromString "d57520b4-5a70-4d40-8623-af2bfbbdfdfe")
    , _questionnaireName = "My VisibleView Questionnaire"
    , _questionnaireDescription = Just "Some description"
    , _questionnaireVisibility = VisibleViewQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedQuestionTagUuids = []
    , _questionnaireProjectTags = [_QUESTIONNAIRE_PROJECT_TAG_1, _QUESTIONNAIRE_PROJECT_TAG_2]
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnairePermissions = [qtn2AlbertEditPermRecord]
    , _questionnaireEvents = fEvents
    , _questionnaireVersions = qVersions
    , _questionnaireIsTemplate = False
    , _questionnaireSquashed = True
    , _questionnaireAnsweredQuestions = 3
    , _questionnaireUnansweredQuestions = 1
    , _questionnaireAppUuid = defaultApp ^. uuid
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    }

questionnaire2Edited :: Questionnaire
questionnaire2Edited =
  Questionnaire
    { _questionnaireUuid = questionnaire2 ^. uuid
    , _questionnaireName = "EDITED: " ++ (questionnaire2 ^. name)
    , _questionnaireDescription = Just "EDITED: Some description"
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = questionnaire2 ^. packageId
    , _questionnaireSelectedQuestionTagUuids = questionnaire2 ^. selectedQuestionTagUuids
    , _questionnaireProjectTags = questionnaire2 ^. projectTags
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnairePermissions = []
    , _questionnaireEvents = questionnaire2 ^. events
    , _questionnaireVersions = questionnaire2 ^. versions
    , _questionnaireIsTemplate = False
    , _questionnaireSquashed = True
    , _questionnaireAnsweredQuestions = 3
    , _questionnaireUnansweredQuestions = 1
    , _questionnaireAppUuid = defaultApp ^. uuid
    , _questionnaireCreatedAt = questionnaire2 ^. createdAt
    , _questionnaireUpdatedAt = questionnaire2 ^. updatedAt
    }

questionnaire2Ctn :: QuestionnaireContent
questionnaire2Ctn =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid = Just $ phase1 ^. uuid
    , _questionnaireContentReplies = fReplies
    , _questionnaireContentCommentThreadsMap = qtnThreads
    , _questionnaireContentLabels = fLabels
    }

questionnaire2ContentEdited :: Questionnaire
questionnaire2ContentEdited = questionnaire2 {_questionnaireEvents = fEventsEdited}

questionnaire2Dto :: QuestionnaireDTO
questionnaire2Dto = toSimpleDTO questionnaire2 germanyPackage QSDefault [qtn2AlbertEditPermRecordDto]

qtn2AlbertEditPermRecord :: QuestionnairePermRecord
qtn2AlbertEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "e3989bdc-d176-408a-afb9-b9259ceb0236"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire2 ^. uuid
    , _questionnairePermRecordMember = albertMember
    , _questionnairePermRecordPerms = ownerPermissions
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
    { _questionnaireUuid = fromJust (U.fromString "16530a07-e673-4ff3-ac1f-57250f2c1bfe")
    , _questionnaireName = "My VisibleEdit Questionnaire"
    , _questionnaireDescription = Just "Some description"
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedQuestionTagUuids = []
    , _questionnaireProjectTags = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Nothing
    , _questionnairePermissions = []
    , _questionnaireEvents = fEvents
    , _questionnaireVersions = qVersions
    , _questionnaireIsTemplate = False
    , _questionnaireSquashed = True
    , _questionnaireAnsweredQuestions = 3
    , _questionnaireUnansweredQuestions = 1
    , _questionnaireAppUuid = defaultApp ^. uuid
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 28) 0
    }

questionnaire3Ctn :: QuestionnaireContent
questionnaire3Ctn =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid = Just $ phase1 ^. uuid
    , _questionnaireContentReplies = fReplies
    , _questionnaireContentCommentThreadsMap = qtnThreads
    , _questionnaireContentLabels = fLabels
    }

questionnaire3ContentEdited :: Questionnaire
questionnaire3ContentEdited = questionnaire1 {_questionnaireEvents = fEventsEdited}

questionnaire3Dto :: QuestionnaireDTO
questionnaire3Dto = toSimpleDTO questionnaire3 germanyPackage QSDefault []

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire4 :: Questionnaire
questionnaire4 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "57250a07-a663-4ff3-ac1f-16530f2c1bfe")
    , _questionnaireName = "Outdated Questionnaire"
    , _questionnaireDescription = Just "Some description"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = netherlandsPackage ^. pId
    , _questionnaireSelectedQuestionTagUuids = []
    , _questionnaireProjectTags = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Nothing
    , _questionnairePermissions = []
    , _questionnaireEvents = [sphse_2']
    , _questionnaireVersions = []
    , _questionnaireIsTemplate = False
    , _questionnaireSquashed = True
    , _questionnaireAnsweredQuestions = 3
    , _questionnaireUnansweredQuestions = 1
    , _questionnaireAppUuid = defaultApp ^. uuid
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire4Ctn :: QuestionnaireContent
questionnaire4Ctn =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid = Just $ phase2 ^. uuid
    , _questionnaireContentReplies = M.empty
    , _questionnaireContentCommentThreadsMap = M.empty
    , _questionnaireContentLabels = M.empty
    }

questionnaire4VisibleView :: Questionnaire
questionnaire4VisibleView = questionnaire4 {_questionnaireVisibility = VisibleViewQuestionnaire}

questionnaire4VisibleEdit :: Questionnaire
questionnaire4VisibleEdit = questionnaire4 {_questionnaireVisibility = VisibleEditQuestionnaire}

questionnaire4Upgraded :: Questionnaire
questionnaire4Upgraded =
  questionnaire4
    { _questionnaireUuid = fromJust (U.fromString "5deabef8-f526-421c-90e2-dd7aed1a25c5")
    , _questionnairePackageId = netherlandsPackageV2 ^. pId
    }

questionnaire4VisibleViewUpgraded :: Questionnaire
questionnaire4VisibleViewUpgraded = questionnaire4Upgraded {_questionnaireVisibility = VisibleViewQuestionnaire}

questionnaire4VisibleEditUpgraded :: Questionnaire
questionnaire4VisibleEditUpgraded = questionnaire4Upgraded {_questionnaireVisibility = VisibleEditQuestionnaire}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire5 :: Questionnaire
questionnaire5 =
  questionnaire1
    { _questionnaireUuid = fromJust (U.fromString "506be867-ba92-4e10-8175-187e99613366")
    , _questionnaireName = "My Private Questionnaire SharedView"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkViewQuestionnaire
    , _questionnairePermissions = [qtn5AlbertEditPermRecord]
    }

questionnaire5ContentEdited :: Questionnaire
questionnaire5ContentEdited = questionnaire5 {_questionnaireEvents = fEventsEdited}

qtn5AlbertEditPermRecord :: QuestionnairePermRecord
qtn5AlbertEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "0ceedc3e-0165-4e7d-ae7c-ac233b7284de"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire5 ^. uuid
    , _questionnairePermRecordMember = albertMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire6 :: Questionnaire
questionnaire6 =
  questionnaire1
    { _questionnaireUuid = fromJust (U.fromString "09304abd-2035-4046-8dc8-b3e5ba8c016c")
    , _questionnaireName = "My Private Questionnaire SharedEdit"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkEditQuestionnaire
    , _questionnairePermissions = [qtn6AlbertEditPermRecord]
    }

questionnaire6Ctn :: QuestionnaireContent
questionnaire6Ctn =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid = Just $ phase1 ^. uuid
    , _questionnaireContentReplies = fReplies
    , _questionnaireContentCommentThreadsMap = qtnThreads
    , _questionnaireContentLabels = fLabels
    }

questionnaire6ContentEdited :: Questionnaire
questionnaire6ContentEdited = questionnaire6 {_questionnaireEvents = fEventsEdited}

questionnaire6Dto :: QuestionnaireDTO
questionnaire6Dto = toSimpleDTO questionnaire6 germanyPackage QSDefault [qtn6AlbertEditPermRecordDto]

qtn6AlbertEditPermRecord :: QuestionnairePermRecord
qtn6AlbertEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "305bf5d7-75a1-4dc4-85af-c092f5010e82"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire6 ^. uuid
    , _questionnairePermRecordMember = albertMember
    , _questionnairePermRecordPerms = ownerPermissions
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
    { _questionnaireUuid = fromJust (U.fromString "abd22b10-63fd-4cb8-bb23-7997ff32eccc")
    , _questionnaireName = "My VisibleView Questionnaire SharedView"
    , _questionnaireVisibility = VisibleViewQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkViewQuestionnaire
    , _questionnairePermissions = [qtn7AlbertEditPermRecord]
    }

questionnaire7ContentEdited :: Questionnaire
questionnaire7ContentEdited = questionnaire7 {_questionnaireEvents = fEventsEdited}

questionnaire7Ctn :: QuestionnaireContent
questionnaire7Ctn =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid = Just $ phase1 ^. uuid
    , _questionnaireContentReplies = fReplies
    , _questionnaireContentCommentThreadsMap = qtnThreads
    , _questionnaireContentLabels = fLabels
    }

qtn7AlbertEditPermRecord :: QuestionnairePermRecord
qtn7AlbertEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "6698fb11-9aac-49a8-867a-a038cff3dded"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire7 ^. uuid
    , _questionnairePermRecordMember = albertMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

qtn7AlbertEditPermRecordDto :: QuestionnairePermRecordDTO
qtn7AlbertEditPermRecordDto = toUserPermRecordDTO qtn7AlbertEditPermRecord userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire8 :: Questionnaire
questionnaire8 =
  questionnaire2
    { _questionnaireUuid = fromJust (U.fromString "a990f62a-ca1f-4517-82d4-399951b8630b")
    , _questionnaireName = "My VisibleView Questionnaire SharedEdit"
    , _questionnaireVisibility = VisibleViewQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkEditQuestionnaire
    , _questionnairePermissions = [qtn8AlbertEditPermRecord]
    }

questionnaire8ContentEdited :: Questionnaire
questionnaire8ContentEdited = questionnaire8 {_questionnaireEvents = fEventsEdited}

qtn8AlbertEditPermRecord :: QuestionnairePermRecord
qtn8AlbertEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "ce6b59d0-c551-4d8e-af0e-40d633238f1d"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire8 ^. uuid
    , _questionnairePermRecordMember = albertMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire9 :: Questionnaire
questionnaire9 =
  questionnaire2
    { _questionnaireUuid = fromJust (U.fromString "936e852f-4c41-4524-8387-bd87090e9fcc")
    , _questionnaireName = "My VisibleEdit Questionnaire SharedView"
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkViewQuestionnaire
    , _questionnairePermissions = [qtn9AlbertEditPermRecord]
    }

questionnaire9ContentEdited :: Questionnaire
questionnaire9ContentEdited = questionnaire9 {_questionnaireEvents = fEventsEdited}

qtn9AlbertEditPermRecord :: QuestionnairePermRecord
qtn9AlbertEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "beb45ac6-a0f4-425b-9d88-fd62ec39baa9"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire9 ^. uuid
    , _questionnairePermRecordMember = albertMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire10 :: Questionnaire
questionnaire10 =
  questionnaire3
    { _questionnaireUuid = fromJust (U.fromString "3c8e7ce6-cb5e-4cd1-a4d1-fb9de55f67ed")
    , _questionnaireName = "My VisibleEdit Questionnaire SharedEdit"
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkEditQuestionnaire
    }

questionnaire10ContentEdited :: Questionnaire
questionnaire10ContentEdited = questionnaire10 {_questionnaireEvents = fEvents ++ [slble_rQ2' & createdBy' .~ Nothing]}

questionnaire10Ctn :: QuestionnaireContent
questionnaire10Ctn =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid = Just $ phase1 ^. uuid
    , _questionnaireContentReplies = fReplies
    , _questionnaireContentCommentThreadsMap = qtnThreads
    , _questionnaireContentLabels = fLabels
    }

questionnaire10Edited :: Questionnaire
questionnaire10Edited = questionnaire10 {_questionnairePermissions = [qtn10NikolaEditPermRecord]}

qtn10NikolaEditPermRecord :: QuestionnairePermRecord
qtn10NikolaEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "93c0e6c0-5aa0-4feb-b5cd-e54f60bdadbf"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire10 ^. uuid
    , _questionnairePermRecordMember = nikolaMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

qtn10NikolaEditPermRecordDto :: QuestionnairePermRecordDTO
qtn10NikolaEditPermRecordDto = toUserPermRecordDTO qtn10NikolaEditPermRecord userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire11 :: Questionnaire
questionnaire11 =
  questionnaire1
    { _questionnaireUuid = fromJust (U.fromString "ba6b6c0e-2bb7-40e7-9019-feb943756888")
    , _questionnaireName = "My Questionnaire from project template"
    , _questionnairePermissions = [qtn11AlbertEditPermRecord]
    }

questionnaire11Ctn :: QuestionnaireContent
questionnaire11Ctn = questionnaire1Ctn

questionnaire11Dto :: QuestionnaireDTO
questionnaire11Dto = toSimpleDTO questionnaire11 germanyPackage QSDefault [qtn11AlbertEditPermRecordDto]

qtn11AlbertEditPermRecord :: QuestionnairePermRecord
qtn11AlbertEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "74e5f2ac-8497-4744-96e2-66325f46d8d2"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire11 ^. uuid
    , _questionnairePermRecordMember = albertMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

qtn11AlbertEditPermRecordDto :: QuestionnairePermRecordDTO
qtn11AlbertEditPermRecordDto = toUserPermRecordDTO qtn11AlbertEditPermRecord userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire12 :: Questionnaire
questionnaire12 =
  questionnaire1
    { _questionnaireUuid = u' "e02bc040-7446-48a2-b557-678e01d66937"
    , _questionnaireName = "My Private Questionnaire with 2 users"
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkEditQuestionnaire
    , _questionnairePermissions = [qtn12AlbertEditPermRecord, qtn12NikolaEditPermRecord]
    }

questionnaire12Ctn :: QuestionnaireContent
questionnaire12Ctn = questionnaire1Ctn

questionnaire12Dto :: QuestionnaireDTO
questionnaire12Dto =
  toSimpleDTO questionnaire12 germanyPackage QSDefault [qtn12AlbertEditPermRecordDto, qtn12NikolaEditPermRecordDto]

qtn12AlbertEditPermRecord :: QuestionnairePermRecord
qtn12AlbertEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "3c268c5a-6778-447c-bf66-4576470eedaf"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire12 ^. uuid
    , _questionnairePermRecordMember = albertMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

qtn12AlbertEditPermRecordDto :: QuestionnairePermRecordDTO
qtn12AlbertEditPermRecordDto = toUserPermRecordDTO qtn12AlbertEditPermRecord userAlbert

qtn12NikolaEditPermRecord :: QuestionnairePermRecord
qtn12NikolaEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "6e3b9e11-f0c8-41ce-aece-5115ddfb63b8"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire12 ^. uuid
    , _questionnairePermRecordMember = nikolaMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

qtn12NikolaEditPermRecordDto :: QuestionnairePermRecordDTO
qtn12NikolaEditPermRecordDto = toUserPermRecordDTO qtn12NikolaEditPermRecord userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire13 :: Questionnaire
questionnaire13 =
  questionnaire1
    { _questionnaireUuid = u' "59b97a8e-aa48-47f7-93a7-646f9df077df"
    , _questionnaireName = "My VisibleCommentQuestionnaire Questionnaire"
    , _questionnaireVisibility = VisibleCommentQuestionnaire
    , _questionnairePermissions = [qtn13NikolaCommentPermRecord]
    }

questionnaire13Ctn :: QuestionnaireContent
questionnaire13Ctn = questionnaire1Ctn

questionnaire13Dto :: QuestionnaireDTO
questionnaire13Dto = toSimpleDTO questionnaire13 germanyPackage QSDefault [qtn13NikolaCommentPermRecordDto]

qtn13NikolaCommentPermRecord :: QuestionnairePermRecord
qtn13NikolaCommentPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "b576befb-0c89-41da-a297-d174133362a2"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire13 ^. uuid
    , _questionnairePermRecordMember = nikolaMember
    , _questionnairePermRecordPerms = commentatorPermissions
    }

qtn13NikolaCommentPermRecordDto :: QuestionnairePermRecordDTO
qtn13NikolaCommentPermRecordDto = toUserPermRecordDTO qtn13NikolaCommentPermRecord userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
differentQuestionnaire :: Questionnaire
differentQuestionnaire =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "7bf4a83e-1687-4e99-b1df-9221977d7b4f")
    , _questionnaireName = "My Different Questionnaire"
    , _questionnaireDescription = Just "Some description"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = differentPackage ^. pId
    , _questionnaireSelectedQuestionTagUuids = []
    , _questionnaireProjectTags = []
    , _questionnaireTemplateId = Just $ anotherWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Just $ userCharles ^. uuid
    , _questionnairePermissions = [differentQtnCharlesOwnerPermRecord]
    , _questionnaireEvents = []
    , _questionnaireVersions = []
    , _questionnaireIsTemplate = True
    , _questionnaireSquashed = True
    , _questionnaireAnsweredQuestions = 3
    , _questionnaireUnansweredQuestions = 1
    , _questionnaireAppUuid = differentApp ^. uuid
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentQtnCharlesOwnerPermRecord :: QuestionnairePermRecord
differentQtnCharlesOwnerPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "e3476103-428e-4729-8f73-adabaad7ef8c"
    , _questionnairePermRecordQuestionnaireUuid = differentQuestionnaire ^. uuid
    , _questionnairePermRecordMember = charlesMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
contentChangeDTO :: QuestionnaireContentChangeDTO
contentChangeDTO =
  QuestionnaireContentChangeDTO
    {_questionnaireContentChangeDTOEvents = fmap (`toEventChangeDTO` samplePhasesAnsweredIndication) fEvents}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
bioGroupEditPermRecord :: QuestionnairePermRecord
bioGroupEditPermRecord =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = u' "637d5a26-20c8-4704-8cee-c3cacff40ea4"
    , _questionnairePermRecordQuestionnaireUuid = questionnaire1 ^. uuid
    , _questionnairePermRecordMember = bioGroupMember
    , _questionnairePermRecordPerms = ownerPermissions
    }

bioGroupEditPermRecordDto :: QuestionnairePermRecordDTO
bioGroupEditPermRecordDto = toGroupPermRecordDTO bioGroupEditPermRecord bioGroup
