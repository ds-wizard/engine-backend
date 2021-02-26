module Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.Migration.Development.Acl.Data.Groups
import Wizard.Database.Migration.Development.Acl.Data.Members
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.QuestionnaireMapper

questionnaire1 :: Questionnaire
questionnaire1 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "af984a75-56e3-49f8-b16f-d6b99599910a")
    , _questionnaireName = "My Private Questionnaire"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnairePermissions = [albertEditPermRecord]
    , _questionnaireEvents = fEvents
    , _questionnaireVersions = qVersions
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire1Edited :: Questionnaire
questionnaire1Edited =
  questionnaire1
    { _questionnaireName = "EDITED: " ++ (questionnaire1 ^. name)
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePermissions = []
    }

questionnaire1ContentEdited :: Questionnaire
questionnaire1ContentEdited = questionnaire1 {_questionnaireEvents = fEventsEdited}

questionnaire1Ctn :: QuestionnaireContent
questionnaire1Ctn =
  QuestionnaireContent
    {_questionnaireContentLevel = 1, _questionnaireContentReplies = fReplies, _questionnaireContentLabels = fLabels}

questionnaire1CtnRevertedDto :: QuestionnaireContentDTO
questionnaire1CtnRevertedDto =
  QuestionnaireContentDTO
    { _questionnaireContentDTOLevel = 1
    , _questionnaireContentDTOReplies = M.fromList [rQ1, rQ2]
    , _questionnaireContentDTOLabels = M.empty
    , _questionnaireContentDTOEvents = [toEventDTO sre_rQ1' (Just userAlbert), toEventDTO sre_rQ2' (Just userAlbert)]
    , _questionnaireContentDTOVersions = []
    }

questionnaire1Dto :: QuestionnaireDTO
questionnaire1Dto =
  toSimpleDTO questionnaire1 questionnaire1Ctn germanyPackage QSDefault questionnaireReport [albertEditPermRecordDto]

questionnaire1Create :: QuestionnaireCreateDTO
questionnaire1Create =
  QuestionnaireCreateDTO
    { _questionnaireCreateDTOName = questionnaire1 ^. name
    , _questionnaireCreateDTOPackageId = questionnaire1 ^. packageId
    , _questionnaireCreateDTOVisibility = questionnaire1 ^. visibility
    , _questionnaireCreateDTOSharing = questionnaire1 ^. sharing
    , _questionnaireCreateDTOTagUuids = []
    , _questionnaireCreateDTOTemplateId = questionnaire1 ^. templateId
    , _questionnaireCreateDTOFormatUuid = questionnaire1 ^. formatUuid
    }

questionnaire1EditedChange :: QuestionnaireChangeDTO
questionnaire1EditedChange =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName = questionnaire1Edited ^. name
    , _questionnaireChangeDTOVisibility = questionnaire1Edited ^. visibility
    , _questionnaireChangeDTOSharing = questionnaire1Edited ^. sharing
    , _questionnaireChangeDTOPermissions = questionnaire1Edited ^. permissions
    , _questionnaireChangeDTOTemplateId = Nothing
    , _questionnaireChangeDTOFormatUuid = Nothing
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire2 :: Questionnaire
questionnaire2 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "d57520b4-5a70-4d40-8623-af2bfbbdfdfe")
    , _questionnaireName = "My VisibleView Questionnaire"
    , _questionnaireVisibility = VisibleViewQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnairePermissions = [albertEditPermRecord]
    , _questionnaireEvents = fEvents
    , _questionnaireVersions = qVersions
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    }

questionnaire2Edited :: Questionnaire
questionnaire2Edited =
  Questionnaire
    { _questionnaireUuid = questionnaire2 ^. uuid
    , _questionnaireName = "EDITED: " ++ (questionnaire2 ^. name)
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = questionnaire2 ^. packageId
    , _questionnaireSelectedTagUuids = questionnaire2 ^. selectedTagUuids
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Just $ userAlbert ^. uuid
    , _questionnairePermissions = []
    , _questionnaireEvents = questionnaire2 ^. events
    , _questionnaireVersions = questionnaire2 ^. versions
    , _questionnaireCreatedAt = questionnaire2 ^. createdAt
    , _questionnaireUpdatedAt = questionnaire2 ^. updatedAt
    }

questionnaire2Ctn :: QuestionnaireContent
questionnaire2Ctn =
  QuestionnaireContent
    {_questionnaireContentLevel = 1, _questionnaireContentReplies = fReplies, _questionnaireContentLabels = fLabels}

questionnaire2ContentEdited :: Questionnaire
questionnaire2ContentEdited = questionnaire2 {_questionnaireEvents = fEventsEdited}

questionnaire2Dto :: QuestionnaireDTO
questionnaire2Dto =
  toSimpleDTO questionnaire2 questionnaire2Ctn germanyPackage QSDefault questionnaireReport [albertEditPermRecordDto]

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire3 :: Questionnaire
questionnaire3 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "16530a07-e673-4ff3-ac1f-57250f2c1bfe")
    , _questionnaireName = "My VisibleEdit Questionnaire"
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = germanyPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Nothing
    , _questionnairePermissions = []
    , _questionnaireEvents = fEvents
    , _questionnaireVersions = qVersions
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 28) 0
    }

questionnaire3Edited :: Questionnaire
questionnaire3Edited =
  Questionnaire
    { _questionnaireUuid = questionnaire3 ^. uuid
    , _questionnaireName = "EDITED: " ++ (questionnaire3 ^. name)
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = questionnaire3 ^. packageId
    , _questionnaireSelectedTagUuids = questionnaire3 ^. selectedTagUuids
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Nothing
    , _questionnairePermissions = [albertEditPermRecord]
    , _questionnaireEvents = questionnaire3 ^. events
    , _questionnaireVersions = questionnaire3 ^. versions
    , _questionnaireCreatedAt = questionnaire3 ^. createdAt
    , _questionnaireUpdatedAt = questionnaire3 ^. updatedAt
    }

questionnaire3Ctn :: QuestionnaireContent
questionnaire3Ctn =
  QuestionnaireContent
    {_questionnaireContentLevel = 1, _questionnaireContentReplies = fReplies, _questionnaireContentLabels = fLabels}

questionnaire3ContentEdited :: Questionnaire
questionnaire3ContentEdited = questionnaire1 {_questionnaireEvents = fEventsEdited}

questionnaire3Dto :: QuestionnaireDTO
questionnaire3Dto = toSimpleDTO questionnaire3 questionnaire3Ctn germanyPackage QSDefault questionnaireReport []

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire4 :: Questionnaire
questionnaire4 =
  Questionnaire
    { _questionnaireUuid = fromJust (U.fromString "57250a07-a663-4ff3-ac1f-16530f2c1bfe")
    , _questionnaireName = "Outdated Questionnaire"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = RestrictedQuestionnaire
    , _questionnairePackageId = netherlandsPackage ^. pId
    , _questionnaireSelectedTagUuids = []
    , _questionnaireTemplateId = Just $ commonWizardTemplate ^. tId
    , _questionnaireFormatUuid = Just $ templateFormatJson ^. uuid
    , _questionnaireCreatorUuid = Nothing
    , _questionnairePermissions = []
    , _questionnaireEvents = [slvle_2']
    , _questionnaireVersions = []
    , _questionnaireCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _questionnaireUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

questionnaire4Ctn :: QuestionnaireContent
questionnaire4Ctn =
  QuestionnaireContent
    {_questionnaireContentLevel = 2, _questionnaireContentReplies = M.empty, _questionnaireContentLabels = M.empty}

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
    }

questionnaire5ContentEdited :: Questionnaire
questionnaire5ContentEdited = questionnaire5 {_questionnaireEvents = fEventsEdited}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire6 :: Questionnaire
questionnaire6 =
  questionnaire1
    { _questionnaireUuid = fromJust (U.fromString "09304abd-2035-4046-8dc8-b3e5ba8c016c")
    , _questionnaireName = "My Private Questionnaire SharedEdit"
    , _questionnaireVisibility = PrivateQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkEditQuestionnaire
    }

questionnaire6Ctn :: QuestionnaireContent
questionnaire6Ctn =
  QuestionnaireContent
    {_questionnaireContentLevel = 1, _questionnaireContentReplies = fReplies, _questionnaireContentLabels = fLabels}

questionnaire6ContentEdited :: Questionnaire
questionnaire6ContentEdited = questionnaire6 {_questionnaireEvents = fEventsEdited}

questionnaire6Dto :: QuestionnaireDTO
questionnaire6Dto =
  toSimpleDTO questionnaire6 questionnaire6Ctn germanyPackage QSDefault questionnaireReport [albertEditPermRecordDto]

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire7 :: Questionnaire
questionnaire7 =
  questionnaire2
    { _questionnaireUuid = fromJust (U.fromString "abd22b10-63fd-4cb8-bb23-7997ff32eccc")
    , _questionnaireName = "My VisibleView Questionnaire SharedView"
    , _questionnaireVisibility = VisibleViewQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkViewQuestionnaire
    }

questionnaire7ContentEdited :: Questionnaire
questionnaire7ContentEdited = questionnaire7 {_questionnaireEvents = fEventsEdited}

questionnaire7Ctn :: QuestionnaireContent
questionnaire7Ctn =
  QuestionnaireContent
    {_questionnaireContentLevel = 1, _questionnaireContentReplies = fReplies, _questionnaireContentLabels = fLabels}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire8 :: Questionnaire
questionnaire8 =
  questionnaire2
    { _questionnaireUuid = fromJust (U.fromString "a990f62a-ca1f-4517-82d4-399951b8630b")
    , _questionnaireName = "My VisibleView Questionnaire SharedEdit"
    , _questionnaireVisibility = VisibleViewQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkEditQuestionnaire
    }

questionnaire8ContentEdited :: Questionnaire
questionnaire8ContentEdited = questionnaire8 {_questionnaireEvents = fEventsEdited}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
questionnaire9 :: Questionnaire
questionnaire9 =
  questionnaire2
    { _questionnaireUuid = fromJust (U.fromString "936e852f-4c41-4524-8387-bd87090e9fcc")
    , _questionnaireName = "My VisibleEdit Questionnaire SharedView"
    , _questionnaireVisibility = VisibleEditQuestionnaire
    , _questionnaireSharing = AnyoneWithLinkViewQuestionnaire
    }

questionnaire9ContentEdited :: Questionnaire
questionnaire9ContentEdited = questionnaire9 {_questionnaireEvents = fEventsEdited}

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
questionnaire10ContentEdited = questionnaire10 {_questionnaireEvents = fEventsEdited}

questionnaire10Ctn :: QuestionnaireContent
questionnaire10Ctn =
  QuestionnaireContent
    {_questionnaireContentLevel = 1, _questionnaireContentReplies = fReplies, _questionnaireContentLabels = fLabels}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
contentChangeDTO :: QuestionnaireContentChangeDTO
contentChangeDTO = QuestionnaireContentChangeDTO {_questionnaireContentChangeDTOEvents = fmap toEventChangeDTO fEvents}

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
bioGroupEditPermRecord :: QuestionnairePermRecord
bioGroupEditPermRecord =
  QuestionnairePermRecord
    {_questionnairePermRecordMember = bioGroupMember, _questionnairePermRecordPerms = ownerPermissions}

bioGroupEditPermRecordDto :: QuestionnairePermRecordDTO
bioGroupEditPermRecordDto = toGroupPermRecordDTO bioGroupEditPermRecord bioGroup

albertEditPermRecord :: QuestionnairePermRecord
albertEditPermRecord =
  QuestionnairePermRecord
    {_questionnairePermRecordMember = albertMember, _questionnairePermRecordPerms = ownerPermissions}

albertEditPermRecordDto :: QuestionnairePermRecordDTO
albertEditPermRecordDto = toUserPermRecordDTO albertEditPermRecord userAlbert
