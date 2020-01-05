module Shared.Database.Migration.Development.Event.Data.Events where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.EventField
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.Event.Move.MoveEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModelLenses

a_km1 :: AddKnowledgeModelEvent
a_km1 =
  AddKnowledgeModelEvent
    { _addKnowledgeModelEventUuid = fromJust $ U.fromString "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f"
    , _addKnowledgeModelEventParentUuid = U.nil
    , _addKnowledgeModelEventEntityUuid = km1WithoutChaptersAndTagsAndIntegrations ^. uuid
    , _addKnowledgeModelEventName = km1WithoutChaptersAndTagsAndIntegrations ^. name
    }

e_km1 :: EditKnowledgeModelEvent
e_km1 =
  EditKnowledgeModelEvent
    { _editKnowledgeModelEventUuid = fromJust $ U.fromString "8294a55d-642d-416c-879b-5a42a4430c24"
    , _editKnowledgeModelEventParentUuid = U.nil
    , _editKnowledgeModelEventEntityUuid = km1 ^. uuid
    , _editKnowledgeModelEventName = ChangedValue $ km1Edited ^. name
    , _editKnowledgeModelEventChapterUuids = ChangedValue $ km1Edited ^. chapterUuids
    , _editKnowledgeModelEventTagUuids = ChangedValue $ km1Edited ^. tagUuids
    , _editKnowledgeModelEventIntegrationUuids = ChangedValue $ km1Edited ^. integrationUuids
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1 :: AddChapterEvent
a_km1_ch1 =
  AddChapterEvent
    { _addChapterEventUuid = fromJust $ U.fromString "dedc4a9d-00d9-41b6-8494-a10a238be03b"
    , _addChapterEventParentUuid = km1 ^. uuid
    , _addChapterEventEntityUuid = chapter1WithoutQuestions ^. uuid
    , _addChapterEventTitle = chapter1WithoutQuestions ^. title
    , _addChapterEventText = chapter1WithoutQuestions ^. text
    }

a_km1_ch2 :: AddChapterEvent
a_km1_ch2 =
  AddChapterEvent
    { _addChapterEventUuid = fromJust $ U.fromString "6c4bba6e-864b-4871-98ca-49ac7a3e5eb5"
    , _addChapterEventParentUuid = km1 ^. uuid
    , _addChapterEventEntityUuid = chapter2WithoutQuestions ^. uuid
    , _addChapterEventTitle = chapter2WithoutQuestions ^. title
    , _addChapterEventText = chapter2WithoutQuestions ^. text
    }

a_km1_ch3 :: AddChapterEvent
a_km1_ch3 =
  AddChapterEvent
    { _addChapterEventUuid = fromJust $ U.fromString "6eaa2b47-711d-4187-98f8-fccdce94db9b"
    , _addChapterEventParentUuid = km1 ^. uuid
    , _addChapterEventEntityUuid = chapter3 ^. uuid
    , _addChapterEventTitle = chapter3 ^. title
    , _addChapterEventText = chapter3 ^. text
    }

a_km1_ch4 :: AddChapterEvent
a_km1_ch4 =
  AddChapterEvent
    { _addChapterEventUuid = fromJust $ U.fromString "6585a64d-c75b-47fc-a86e-e0c8e773528f"
    , _addChapterEventParentUuid = km1 ^. uuid
    , _addChapterEventEntityUuid = chapter4WithoutQuestions ^. uuid
    , _addChapterEventTitle = chapter4WithoutQuestions ^. title
    , _addChapterEventText = chapter4WithoutQuestions ^. text
    }

e_km1_ch1 :: EditChapterEvent
e_km1_ch1 =
  EditChapterEvent
    { _editChapterEventUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
    , _editChapterEventParentUuid = km1 ^. uuid
    , _editChapterEventEntityUuid = chapter1 ^. uuid
    , _editChapterEventTitle = ChangedValue $ chapter1Edited ^. title
    , _editChapterEventText = ChangedValue $ chapter1Edited ^. text
    , _editChapterEventQuestionUuids = ChangedValue $ chapter1Edited ^. questionUuids
    }

e_km1_ch1_2 :: EditChapterEvent
e_km1_ch1_2 =
  EditChapterEvent
    { _editChapterEventUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
    , _editChapterEventParentUuid = km1 ^. uuid
    , _editChapterEventEntityUuid = chapter1 ^. uuid
    , _editChapterEventTitle = ChangedValue $ "TWICE: " ++ chapter1Edited ^. title
    , _editChapterEventText = ChangedValue $ chapter1Edited ^. text
    , _editChapterEventQuestionUuids = ChangedValue $ chapter1Edited ^. questionUuids
    }

d_km1_ch1 :: DeleteChapterEvent
d_km1_ch1 =
  DeleteChapterEvent
    { _deleteChapterEventUuid = fromJust $ U.fromString "d07cc69b-abd3-43ec-bce1-fe59899dbda3"
    , _deleteChapterEventParentUuid = km1 ^. uuid
    , _deleteChapterEventEntityUuid = chapter1 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q1' :: AddQuestionEvent
a_km1_ch1_q1' = AddValueQuestionEvent' a_km1_ch1_q1

a_km1_ch1_q1 :: AddValueQuestionEvent
a_km1_ch1_q1 =
  AddValueQuestionEvent
    { _addValueQuestionEventUuid = fromJust $ U.fromString "71ae2ce9-553b-4ca2-a542-1bce04406c51"
    , _addValueQuestionEventParentUuid = chapter1 ^. uuid
    , _addValueQuestionEventEntityUuid = question1 ^. uuid
    , _addValueQuestionEventTitle = question1 ^. title
    , _addValueQuestionEventText = question1 ^. text
    , _addValueQuestionEventRequiredLevel = question1 ^. requiredLevel
    , _addValueQuestionEventTagUuids = question1 ^. tagUuids
    , _addValueQuestionEventValueType = question1 ^. valueType
    }

a_km1_ch1_q2' :: AddQuestionEvent
a_km1_ch1_q2' = AddOptionsQuestionEvent' a_km1_ch1_q2

a_km1_ch1_q2 :: AddOptionsQuestionEvent
a_km1_ch1_q2 =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = fromJust $ U.fromString "ced9be29-24af-4443-8f5f-e709791a8fe3"
    , _addOptionsQuestionEventParentUuid = chapter1 ^. uuid
    , _addOptionsQuestionEventEntityUuid = question2 ^. uuid
    , _addOptionsQuestionEventTitle = question2 ^. title
    , _addOptionsQuestionEventText = question2 ^. text
    , _addOptionsQuestionEventRequiredLevel = question2 ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = question2 ^. tagUuids
    }

a_km1_ch1_q3' :: AddQuestionEvent
a_km1_ch1_q3' = AddOptionsQuestionEvent' a_km1_ch1_q3

a_km1_ch1_q3 :: AddOptionsQuestionEvent
a_km1_ch1_q3 =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = fromJust $ U.fromString "d559ac95-cc81-4502-a780-dbaee46f24bc"
    , _addOptionsQuestionEventParentUuid = chapter1 ^. uuid
    , _addOptionsQuestionEventEntityUuid = question3 ^. uuid
    , _addOptionsQuestionEventTitle = question3 ^. title
    , _addOptionsQuestionEventText = question3 ^. text
    , _addOptionsQuestionEventRequiredLevel = question3 ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = question3 ^. tagUuids
    }

a_km1_ch2_q3' :: AddQuestionEvent
a_km1_ch2_q3' = AddOptionsQuestionEvent' a_km1_ch2_q3

a_km1_ch2_q3 :: AddOptionsQuestionEvent
a_km1_ch2_q3 =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
    , _addOptionsQuestionEventParentUuid = chapter2 ^. uuid
    , _addOptionsQuestionEventEntityUuid = question3 ^. uuid
    , _addOptionsQuestionEventTitle = question3 ^. title
    , _addOptionsQuestionEventText = question3 ^. text
    , _addOptionsQuestionEventRequiredLevel = question3 ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = question3 ^. tagUuids
    }

a_km1_ch2_q4' :: AddQuestionEvent
a_km1_ch2_q4' = AddListQuestionEvent' a_km1_ch2_q4

a_km1_ch2_q4 :: AddListQuestionEvent
a_km1_ch2_q4 =
  AddListQuestionEvent
    { _addListQuestionEventUuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
    , _addListQuestionEventParentUuid = chapter2 ^. uuid
    , _addListQuestionEventEntityUuid = question4 ^. uuid
    , _addListQuestionEventTitle = question4 ^. title
    , _addListQuestionEventText = question4 ^. text
    , _addListQuestionEventRequiredLevel = question4 ^. requiredLevel
    , _addListQuestionEventTagUuids = question4 ^. tagUuids
    }

a_km1_ch3_q9' :: AddQuestionEvent
a_km1_ch3_q9' = AddIntegrationQuestionEvent' a_km1_ch3_q9

a_km1_ch3_q9 :: AddIntegrationQuestionEvent
a_km1_ch3_q9 =
  AddIntegrationQuestionEvent
    { _addIntegrationQuestionEventUuid = fromJust $ U.fromString "51526318-2727-4113-993d-bae5d4abafcd"
    , _addIntegrationQuestionEventParentUuid = chapter3 ^. uuid
    , _addIntegrationQuestionEventEntityUuid = question9 ^. uuid
    , _addIntegrationQuestionEventTitle = question9 ^. title
    , _addIntegrationQuestionEventText = question9 ^. text
    , _addIntegrationQuestionEventRequiredLevel = question9 ^. requiredLevel
    , _addIntegrationQuestionEventTagUuids = question9 ^. tagUuids
    , _addIntegrationQuestionEventIntegrationUuid = question9 ^. integrationUuid
    , _addIntegrationQuestionEventProps = question9 ^. props
    }

a_km1_ch3_q10' :: AddQuestionEvent
a_km1_ch3_q10' = AddIntegrationQuestionEvent' a_km1_ch3_q10

a_km1_ch3_q10 :: AddIntegrationQuestionEvent
a_km1_ch3_q10 =
  AddIntegrationQuestionEvent
    { _addIntegrationQuestionEventUuid = fromJust $ U.fromString "e8531168-946d-4d95-a3b5-f092d32dee1a"
    , _addIntegrationQuestionEventParentUuid = chapter3 ^. uuid
    , _addIntegrationQuestionEventEntityUuid = question10 ^. uuid
    , _addIntegrationQuestionEventTitle = question10 ^. title
    , _addIntegrationQuestionEventText = question10 ^. text
    , _addIntegrationQuestionEventRequiredLevel = question10 ^. requiredLevel
    , _addIntegrationQuestionEventTagUuids = question10 ^. tagUuids
    , _addIntegrationQuestionEventIntegrationUuid = question10 ^. integrationUuid
    , _addIntegrationQuestionEventProps = question10 ^. props
    }

e_km1_ch1_q1' :: EditQuestionEvent
e_km1_ch1_q1' = EditValueQuestionEvent' e_km1_ch1_q1

e_km1_ch1_q1 :: EditValueQuestionEvent
e_km1_ch1_q1 =
  EditValueQuestionEvent
    { _editValueQuestionEventUuid = fromJust $ U.fromString "de86f82b-aaaf-482e-97c7-c7e93d834cd9"
    , _editValueQuestionEventParentUuid = chapter1 ^. uuid
    , _editValueQuestionEventEntityUuid = question1Edited ^. uuid
    , _editValueQuestionEventTitle = ChangedValue $ question1Edited ^. title
    , _editValueQuestionEventText = NothingChanged
    , _editValueQuestionEventRequiredLevel = NothingChanged
    , _editValueQuestionEventTagUuids = NothingChanged
    , _editValueQuestionEventExpertUuids = NothingChanged
    , _editValueQuestionEventReferenceUuids = NothingChanged
    , _editValueQuestionEventValueType = NothingChanged
    }

e_km1_ch1_q1_type' :: EditQuestionEvent
e_km1_ch1_q1_type' = EditOptionsQuestionEvent' e_km1_ch1_q1_type

e_km1_ch1_q1_type :: EditOptionsQuestionEvent
e_km1_ch1_q1_type =
  EditOptionsQuestionEvent
    { _editOptionsQuestionEventUuid = fromJust $ U.fromString "f56b1435-ec9f-4d79-88b3-04c39b73724d"
    , _editOptionsQuestionEventParentUuid = chapter1 ^. uuid
    , _editOptionsQuestionEventEntityUuid = question1WithNewType ^. uuid
    , _editOptionsQuestionEventTitle = ChangedValue $ question1WithNewType ^. title
    , _editOptionsQuestionEventText = NothingChanged
    , _editOptionsQuestionEventRequiredLevel = NothingChanged
    , _editOptionsQuestionEventTagUuids = NothingChanged
    , _editOptionsQuestionEventExpertUuids = NothingChanged
    , _editOptionsQuestionEventReferenceUuids = NothingChanged
    , _editOptionsQuestionEventAnswerUuids = ChangedValue $ question1WithNewType' ^. answerUuids'
    }

e_km1_ch1_q2' :: EditQuestionEvent
e_km1_ch1_q2' = EditOptionsQuestionEvent' e_km1_ch1_q2

e_km1_ch1_q2 :: EditOptionsQuestionEvent
e_km1_ch1_q2 =
  EditOptionsQuestionEvent
    { _editOptionsQuestionEventUuid = fromJust $ U.fromString "1a01665b-e896-450d-b606-afc1dcca586b"
    , _editOptionsQuestionEventParentUuid = chapter1 ^. uuid
    , _editOptionsQuestionEventEntityUuid = question2 ^. uuid
    , _editOptionsQuestionEventTitle = ChangedValue $ question2Edited ^. title
    , _editOptionsQuestionEventText = ChangedValue $ question2Edited ^. text
    , _editOptionsQuestionEventRequiredLevel = ChangedValue $ question2Edited ^. requiredLevel
    , _editOptionsQuestionEventTagUuids = ChangedValue $ question2Edited ^. tagUuids
    , _editOptionsQuestionEventExpertUuids = ChangedValue $ question2Edited' ^. expertUuids'
    , _editOptionsQuestionEventReferenceUuids = ChangedValue $ question2Edited' ^. referenceUuids'
    , _editOptionsQuestionEventAnswerUuids = ChangedValue $ question2Edited' ^. answerUuids'
    }

e_km1_ch1_q2_second_edit' :: EditQuestionEvent
e_km1_ch1_q2_second_edit' = EditOptionsQuestionEvent' e_km1_ch1_q2_second_edit

e_km1_ch1_q2_second_edit :: EditOptionsQuestionEvent
e_km1_ch1_q2_second_edit =
  EditOptionsQuestionEvent
    { _editOptionsQuestionEventUuid = fromJust $ U.fromString "bf888b95-921d-4caa-88af-3309393d44c3"
    , _editOptionsQuestionEventParentUuid = chapter1 ^. uuid
    , _editOptionsQuestionEventEntityUuid = question2 ^. uuid
    , _editOptionsQuestionEventTitle = ChangedValue "New title"
    , _editOptionsQuestionEventText = ChangedValue $ question2Edited ^. text
    , _editOptionsQuestionEventRequiredLevel = ChangedValue $ question2Edited ^. requiredLevel
    , _editOptionsQuestionEventTagUuids = ChangedValue $ question2Edited ^. tagUuids
    , _editOptionsQuestionEventExpertUuids = ChangedValue $ question2Edited' ^. expertUuids'
    , _editOptionsQuestionEventReferenceUuids = ChangedValue $ question2Edited' ^. referenceUuids'
    , _editOptionsQuestionEventAnswerUuids = ChangedValue $ question2Edited' ^. answerUuids'
    }

e_km1_ch1_q2_type' :: EditQuestionEvent
e_km1_ch1_q2_type' = EditListQuestionEvent' e_km1_ch1_q2_type

e_km1_ch1_q2_type :: EditListQuestionEvent
e_km1_ch1_q2_type =
  EditListQuestionEvent
    { _editListQuestionEventUuid = fromJust $ U.fromString "2727c225-78e5-4d5f-a093-cfaadb6ea663"
    , _editListQuestionEventParentUuid = chapter1 ^. uuid
    , _editListQuestionEventEntityUuid = question2WithNewType ^. uuid
    , _editListQuestionEventTitle = ChangedValue $ question2WithNewType ^. title
    , _editListQuestionEventText = NothingChanged
    , _editListQuestionEventRequiredLevel = NothingChanged
    , _editListQuestionEventTagUuids = NothingChanged
    , _editListQuestionEventExpertUuids = NothingChanged
    , _editListQuestionEventReferenceUuids = NothingChanged
    , _editListQuestionEventItemTemplateQuestionUuids = ChangedValue $ []
    }

e_km1_ch2_q4' :: EditQuestionEvent
e_km1_ch2_q4' = EditListQuestionEvent' e_km1_ch2_q4

e_km1_ch2_q4 :: EditListQuestionEvent
e_km1_ch2_q4 =
  EditListQuestionEvent
    { _editListQuestionEventUuid = fromJust $ U.fromString "7014c6de-a1c0-4c09-881a-c83c68a29de1"
    , _editListQuestionEventParentUuid = chapter2 ^. uuid
    , _editListQuestionEventEntityUuid = question4Edited ^. uuid
    , _editListQuestionEventTitle = ChangedValue $ question4Edited ^. title
    , _editListQuestionEventText = ChangedValue $ question4Edited ^. text
    , _editListQuestionEventRequiredLevel = ChangedValue $ question4Edited ^. requiredLevel
    , _editListQuestionEventTagUuids = ChangedValue $ question4Edited ^. tagUuids
    , _editListQuestionEventExpertUuids = ChangedValue $ question4Edited' ^. expertUuids'
    , _editListQuestionEventReferenceUuids = ChangedValue $ question4Edited' ^. referenceUuids'
    , _editListQuestionEventItemTemplateQuestionUuids = ChangedValue $ question4Edited ^. itemTemplateQuestionUuids
    }

e_km1_ch2_q4_type' :: EditQuestionEvent
e_km1_ch2_q4_type' = EditIntegrationQuestionEvent' e_km1_ch2_q4_type

e_km1_ch2_q4_type :: EditIntegrationQuestionEvent
e_km1_ch2_q4_type =
  EditIntegrationQuestionEvent
    { _editIntegrationQuestionEventUuid = fromJust $ U.fromString "0f6f536c-aa1c-4d47-8cd7-46d611b43a56"
    , _editIntegrationQuestionEventParentUuid = chapter2 ^. uuid
    , _editIntegrationQuestionEventEntityUuid = question4WithNewType ^. uuid
    , _editIntegrationQuestionEventTitle = ChangedValue $ question4WithNewType ^. title
    , _editIntegrationQuestionEventText = NothingChanged
    , _editIntegrationQuestionEventRequiredLevel = NothingChanged
    , _editIntegrationQuestionEventTagUuids = NothingChanged
    , _editIntegrationQuestionEventExpertUuids = NothingChanged
    , _editIntegrationQuestionEventReferenceUuids = NothingChanged
    , _editIntegrationQuestionEventIntegrationUuid = ChangedValue $ question4WithNewType ^. integrationUuid
    , _editIntegrationQuestionEventProps = ChangedValue $ question4WithNewType ^. props
    }

e_km1_ch3_q9' :: EditQuestionEvent
e_km1_ch3_q9' = EditIntegrationQuestionEvent' e_km1_ch3_q9

e_km1_ch3_q9 :: EditIntegrationQuestionEvent
e_km1_ch3_q9 =
  EditIntegrationQuestionEvent
    { _editIntegrationQuestionEventUuid = fromJust $ U.fromString "43779823-507b-41f1-8dce-7c5e0660db8f"
    , _editIntegrationQuestionEventParentUuid = chapter3 ^. uuid
    , _editIntegrationQuestionEventEntityUuid = question9Edited ^. uuid
    , _editIntegrationQuestionEventTitle = ChangedValue $ question9Edited ^. title
    , _editIntegrationQuestionEventText = ChangedValue $ question9Edited ^. text
    , _editIntegrationQuestionEventRequiredLevel = ChangedValue $ question9Edited ^. requiredLevel
    , _editIntegrationQuestionEventTagUuids = ChangedValue $ question9Edited ^. tagUuids
    , _editIntegrationQuestionEventExpertUuids = ChangedValue $ question9Edited' ^. expertUuids'
    , _editIntegrationQuestionEventReferenceUuids = ChangedValue $ question9Edited' ^. referenceUuids'
    , _editIntegrationQuestionEventIntegrationUuid = ChangedValue $ question9Edited ^. integrationUuid
    , _editIntegrationQuestionEventProps = ChangedValue $ question9Edited ^. props
    }

e_km1_ch3_q9_type' :: EditQuestionEvent
e_km1_ch3_q9_type' = EditValueQuestionEvent' e_km1_ch3_q9_type

e_km1_ch3_q9_type :: EditValueQuestionEvent
e_km1_ch3_q9_type =
  EditValueQuestionEvent
    { _editValueQuestionEventUuid = fromJust $ U.fromString "91514dc3-29b1-469a-b0d9-5fc211df1c47"
    , _editValueQuestionEventParentUuid = chapter3 ^. uuid
    , _editValueQuestionEventEntityUuid = question9WithNewType ^. uuid
    , _editValueQuestionEventTitle = ChangedValue $ question9WithNewType ^. title
    , _editValueQuestionEventText = NothingChanged
    , _editValueQuestionEventRequiredLevel = NothingChanged
    , _editValueQuestionEventTagUuids = NothingChanged
    , _editValueQuestionEventExpertUuids = NothingChanged
    , _editValueQuestionEventReferenceUuids = NothingChanged
    , _editValueQuestionEventValueType = ChangedValue $ question9WithNewType ^. valueType
    }

d_km1_ch1_q1 :: DeleteQuestionEvent
d_km1_ch1_q1 =
  DeleteQuestionEvent
    { _deleteQuestionEventUuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
    , _deleteQuestionEventParentUuid = chapter1 ^. uuid
    , _deleteQuestionEventEntityUuid = question1 ^. uuid
    }

d_km1_ch1_q1_2 :: DeleteQuestionEvent
d_km1_ch1_q1_2 =
  DeleteQuestionEvent
    { _deleteQuestionEventUuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
    , _deleteQuestionEventParentUuid = chapter1 ^. uuid
    , _deleteQuestionEventEntityUuid = question1 ^. uuid
    }

d_km1_ch1_q2 :: DeleteQuestionEvent
d_km1_ch1_q2 =
  DeleteQuestionEvent
    { _deleteQuestionEventUuid = fromJust $ U.fromString "52a7a6ae-be37-4075-ac5c-a20858707a75"
    , _deleteQuestionEventParentUuid = chapter1 ^. uuid
    , _deleteQuestionEventEntityUuid = question2 ^. uuid
    }

d_km1_ch1_q3 :: DeleteQuestionEvent
d_km1_ch1_q3 =
  DeleteQuestionEvent
    { _deleteQuestionEventUuid = fromJust $ U.fromString "e46d208f-eb7d-48bc-8187-13a72b17ddb2"
    , _deleteQuestionEventParentUuid = chapter1 ^. uuid
    , _deleteQuestionEventEntityUuid = question3 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_aNo1 :: AddAnswerEvent
a_km1_ch1_q2_aNo1 =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "afb36736-503a-43ca-a56b-8c144f89809e"
    , _addAnswerEventParentUuid = question2 ^. uuid
    , _addAnswerEventEntityUuid = q2_answerNo ^. uuid
    , _addAnswerEventLabel = q2_answerNo ^. label
    , _addAnswerEventAdvice = q2_answerNo ^. advice
    , _addAnswerEventMetricMeasures = q2_answerNo ^. metricMeasures
    }

a_km1_ch1_q2_aYes1 :: AddAnswerEvent
a_km1_ch1_q2_aYes1 =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "e7ee93e4-18e7-4748-b0a5-781c77b8c937"
    , _addAnswerEventParentUuid = question2 ^. uuid
    , _addAnswerEventEntityUuid = q2_answerYes ^. uuid
    , _addAnswerEventLabel = q2_answerYes ^. label
    , _addAnswerEventAdvice = q2_answerYes ^. advice
    , _addAnswerEventMetricMeasures = q2_answerYes ^. metricMeasures
    }

a_km1_ch1_q2_aMaybe :: AddAnswerEvent
a_km1_ch1_q2_aMaybe =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "8ba60993-96ac-496b-9b8c-9580bf992cab"
    , _addAnswerEventParentUuid = question2 ^. uuid
    , _addAnswerEventEntityUuid = q2_answerMaybe ^. uuid
    , _addAnswerEventLabel = q2_answerMaybe ^. label
    , _addAnswerEventAdvice = q2_answerMaybe ^. advice
    , _addAnswerEventMetricMeasures = q2_answerMaybe ^. metricMeasures
    }

a_km1_ch1_q2_aYes1_fuq1_aNo :: AddAnswerEvent
a_km1_ch1_q2_aYes1_fuq1_aNo =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "e62168e2-afe5-4e58-8ee7-555594aec23e"
    , _addAnswerEventParentUuid = q2_aYes_fuQuestion1 ^. uuid
    , _addAnswerEventEntityUuid = q2_aYes_fuq1_answerNo ^. uuid
    , _addAnswerEventLabel = q2_aYes_fuq1_answerNo ^. label
    , _addAnswerEventAdvice = q2_aYes_fuq1_answerNo ^. advice
    , _addAnswerEventMetricMeasures = q2_aYes_fuq1_answerNo ^. metricMeasures
    }

a_km1_ch1_q2_aYesFu1 :: AddAnswerEvent
a_km1_ch1_q2_aYesFu1 =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "bc530681-b45b-4d36-b179-a9cb62a92838"
    , _addAnswerEventParentUuid = q2_aYes_fuQuestion1 ^. uuid
    , _addAnswerEventEntityUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _addAnswerEventLabel = q2_aYes_fuq1_answerYes ^. label
    , _addAnswerEventAdvice = q2_aYes_fuq1_answerYes ^. advice
    , _addAnswerEventMetricMeasures = q2_aYes_fuq1_answerYes ^. metricMeasures
    }

a_km1_ch1_q2_aNoFu2 :: AddAnswerEvent
a_km1_ch1_q2_aNoFu2 =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "abf67af9-23e0-43fa-a54a-746570882624"
    , _addAnswerEventParentUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _addAnswerEventEntityUuid = q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid
    , _addAnswerEventLabel = q2_aYes_fuq1_aYes_fuq2_answerNo ^. label
    , _addAnswerEventAdvice = q2_aYes_fuq1_aYes_fuq2_answerNo ^. advice
    , _addAnswerEventMetricMeasures = q2_aYes_fuq1_aYes_fuq2_answerNo ^. metricMeasures
    }

a_km1_ch1_q2_aYesFu2 :: AddAnswerEvent
a_km1_ch1_q2_aYesFu2 =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "542c0d28-9ae3-4bbe-8030-92a78b462276"
    , _addAnswerEventParentUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _addAnswerEventEntityUuid = q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid
    , _addAnswerEventLabel = q2_aYes_fuq1_aYes_fuq2_answerYes ^. label
    , _addAnswerEventAdvice = q2_aYes_fuq1_aYes_fuq2_answerYes ^. advice
    , _addAnswerEventMetricMeasures = q2_aYes_fuq1_aYes_fuq2_answerYes ^. metricMeasures
    }

a_km1_ch2_q3_aNo2 :: AddAnswerEvent
a_km1_ch2_q3_aNo2 =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "1bb10e82-33b5-4c98-b1d1-ab5413b5df66"
    , _addAnswerEventParentUuid = question3 ^. uuid
    , _addAnswerEventEntityUuid = q3_answerNo ^. uuid
    , _addAnswerEventLabel = q3_answerNo ^. label
    , _addAnswerEventAdvice = q3_answerNo ^. advice
    , _addAnswerEventMetricMeasures = q3_answerNo ^. metricMeasures
    }

a_km1_ch2_q3_aYes2 :: AddAnswerEvent
a_km1_ch2_q3_aYes2 =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "885ea1b9-0041-4240-911c-f35a9a6e4cbd"
    , _addAnswerEventParentUuid = question3 ^. uuid
    , _addAnswerEventEntityUuid = q3_answerYes ^. uuid
    , _addAnswerEventLabel = q3_answerYes ^. label
    , _addAnswerEventAdvice = q3_answerYes ^. advice
    , _addAnswerEventMetricMeasures = q3_answerYes ^. metricMeasures
    }

a_km1_ch2_q4_it_q6_aNo :: AddAnswerEvent
a_km1_ch2_q4_it_q6_aNo =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "c0a67ce5-21b3-47c7-8624-c2da26fb494f"
    , _addAnswerEventParentUuid = q4_it1_question6 ^. uuid
    , _addAnswerEventEntityUuid = q4_it1_q6_answerNo ^. uuid
    , _addAnswerEventLabel = q4_it1_q6_answerNo ^. label
    , _addAnswerEventAdvice = q4_it1_q6_answerNo ^. advice
    , _addAnswerEventMetricMeasures = q4_it1_q6_answerNo ^. metricMeasures
    }

a_km1_ch2_q4_it_q6_aYes :: AddAnswerEvent
a_km1_ch2_q4_it_q6_aYes =
  AddAnswerEvent
    { _addAnswerEventUuid = fromJust $ U.fromString "c5c42f99-613b-4b6c-ae5e-af784f51c483"
    , _addAnswerEventParentUuid = q4_it1_question6 ^. uuid
    , _addAnswerEventEntityUuid = q4_it1_q6_answerYes ^. uuid
    , _addAnswerEventLabel = q4_it1_q6_answerYes ^. label
    , _addAnswerEventAdvice = q4_it1_q6_answerYes ^. advice
    , _addAnswerEventMetricMeasures = q4_it1_q6_answerYes ^. metricMeasures
    }

e_km1_ch1_q2_aYes1 :: EditAnswerEvent
e_km1_ch1_q2_aYes1 =
  EditAnswerEvent
    { _editAnswerEventUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
    , _editAnswerEventParentUuid = question2 ^. uuid
    , _editAnswerEventEntityUuid = q2_answerYes ^. uuid
    , _editAnswerEventLabel = ChangedValue $ q2_answerYesEdited ^. label
    , _editAnswerEventAdvice = ChangedValue $ q2_answerYesEdited ^. advice
    , _editAnswerEventFollowUpUuids = ChangedValue $ q2_answerYesEdited ^. followUpUuids
    , _editAnswerEventMetricMeasures = ChangedValue $ q2_answerYesEdited ^. metricMeasures
    }

e_km1_ch1_q2_aYes1_2 :: EditAnswerEvent
e_km1_ch1_q2_aYes1_2 =
  EditAnswerEvent
    { _editAnswerEventUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
    , _editAnswerEventParentUuid = question2 ^. uuid
    , _editAnswerEventEntityUuid = q2_answerYes ^. uuid
    , _editAnswerEventLabel = ChangedValue $ q2_answerYesEdited ^. label
    , _editAnswerEventAdvice = ChangedValue $ q2_answerYesEdited ^. advice
    , _editAnswerEventFollowUpUuids = ChangedValue $ q2_answerYes ^. followUpUuids
    , _editAnswerEventMetricMeasures = ChangedValue $ q2_answerYes ^. metricMeasures
    }

d_km1_ch1_q2_aYes1 :: DeleteAnswerEvent
d_km1_ch1_q2_aYes1 =
  DeleteAnswerEvent
    { _deleteAnswerEventUuid = fromJust $ U.fromString "1968692f-959a-4d47-b85f-d684eedb3e7f"
    , _deleteAnswerEventParentUuid = question2 ^. uuid
    , _deleteAnswerEventEntityUuid = q2_answerYes ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- AnswerItemTemplateQuestionEvent
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch2_q4_it1_q5' :: AddQuestionEvent
a_km1_ch2_q4_it1_q5' = AddListQuestionEvent' a_km1_ch2_q4_it1_q5

a_km1_ch2_q4_it1_q5 :: AddListQuestionEvent
a_km1_ch2_q4_it1_q5 =
  AddListQuestionEvent
    { _addListQuestionEventUuid = fromJust $ U.fromString "5619d036-0130-47fa-9553-b73094eecd7e"
    , _addListQuestionEventParentUuid = question4 ^. uuid
    , _addListQuestionEventEntityUuid = q4_it1_question5 ^. uuid
    , _addListQuestionEventTitle = q4_it1_question5 ^. title
    , _addListQuestionEventText = q4_it1_question5 ^. text
    , _addListQuestionEventRequiredLevel = q4_it1_question5 ^. requiredLevel
    , _addListQuestionEventTagUuids = q4_it1_question5 ^. tagUuids
    }

a_km1_ch2_q4_it1_q6' :: AddQuestionEvent
a_km1_ch2_q4_it1_q6' = AddOptionsQuestionEvent' a_km1_ch2_q4_it1_q6

a_km1_ch2_q4_it1_q6 :: AddOptionsQuestionEvent
a_km1_ch2_q4_it1_q6 =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = fromJust $ U.fromString "5ac56741-b93a-42f5-9beb-f22100e4342d"
    , _addOptionsQuestionEventParentUuid = question4 ^. uuid
    , _addOptionsQuestionEventEntityUuid = q4_it1_question6 ^. uuid
    , _addOptionsQuestionEventTitle = q4_it1_question6 ^. title
    , _addOptionsQuestionEventText = q4_it1_question6 ^. text
    , _addOptionsQuestionEventRequiredLevel = q4_it1_question6 ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = q4_it1_question6 ^. tagUuids
    }

a_km1_ch2_q4_it1_q6_fuq4_q1' :: AddQuestionEvent
a_km1_ch2_q4_it1_q6_fuq4_q1' = AddOptionsQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q1

a_km1_ch2_q4_it1_q6_fuq4_q1 :: AddOptionsQuestionEvent
a_km1_ch2_q4_it1_q6_fuq4_q1 =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = fromJust $ U.fromString "55f46913-a953-4318-b72f-673e9f65fb2a"
    , _addOptionsQuestionEventParentUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _addOptionsQuestionEventEntityUuid = q4_it1_q6_aYes_fuq4_it_question1 ^. uuid
    , _addOptionsQuestionEventTitle = q4_it1_q6_aYes_fuq4_it_question1 ^. title
    , _addOptionsQuestionEventText = q4_it1_q6_aYes_fuq4_it_question1 ^. text
    , _addOptionsQuestionEventRequiredLevel = q4_it1_q6_aYes_fuq4_it_question1 ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = q4_it1_q6_aYes_fuq4_it_question1 ^. tagUuids
    }

a_km1_ch2_q4_it1_q6_fuq4_q2' :: AddQuestionEvent
a_km1_ch2_q4_it1_q6_fuq4_q2' = AddOptionsQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q2

a_km1_ch2_q4_it1_q6_fuq4_q2 :: AddOptionsQuestionEvent
a_km1_ch2_q4_it1_q6_fuq4_q2 =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = fromJust $ U.fromString "6b9a7c1c-a23e-458a-a1bb-d7500c0ed96e"
    , _addOptionsQuestionEventParentUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _addOptionsQuestionEventEntityUuid = q4_it1_q6_aYes_fuq4_it_question2 ^. uuid
    , _addOptionsQuestionEventTitle = q4_it1_q6_aYes_fuq4_it_question2 ^. title
    , _addOptionsQuestionEventText = q4_it1_q6_aYes_fuq4_it_question2 ^. text
    , _addOptionsQuestionEventRequiredLevel = q4_it1_q6_aYes_fuq4_it_question2 ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = q4_it1_q6_aYes_fuq4_it_question2 ^. tagUuids
    }

a_km1_ch2_q4_it1_q7' :: AddQuestionEvent
a_km1_ch2_q4_it1_q7' = AddValueQuestionEvent' a_km1_ch2_q4_it1_q7

a_km1_ch2_q4_it1_q7 :: AddValueQuestionEvent
a_km1_ch2_q4_it1_q7 =
  AddValueQuestionEvent
    { _addValueQuestionEventUuid = fromJust $ U.fromString "cf839365-91d0-427a-bb99-89de1a125929"
    , _addValueQuestionEventParentUuid = q4_it1_question5 ^. uuid
    , _addValueQuestionEventEntityUuid = q4_it1_q5_it2_question7 ^. uuid
    , _addValueQuestionEventTitle = q4_it1_q5_it2_question7 ^. title
    , _addValueQuestionEventText = q4_it1_q5_it2_question7 ^. text
    , _addValueQuestionEventRequiredLevel = q4_it1_q5_it2_question7 ^. requiredLevel
    , _addValueQuestionEventTagUuids = q4_it1_q5_it2_question7 ^. tagUuids
    , _addValueQuestionEventValueType = q4_it1_q5_it2_question7 ^. valueType
    }

a_km1_ch2_q4_it1_q8' :: AddQuestionEvent
a_km1_ch2_q4_it1_q8' = AddValueQuestionEvent' a_km1_ch2_q4_it1_q8

a_km1_ch2_q4_it1_q8 :: AddValueQuestionEvent
a_km1_ch2_q4_it1_q8 =
  AddValueQuestionEvent
    { _addValueQuestionEventUuid = fromJust $ U.fromString "3536a56f-d19c-4aff-ada1-ef7b3a60389d"
    , _addValueQuestionEventParentUuid = q4_it1_question5 ^. uuid
    , _addValueQuestionEventEntityUuid = q4_it1_q5_it2_question8 ^. uuid
    , _addValueQuestionEventTitle = q4_it1_q5_it2_question8 ^. title
    , _addValueQuestionEventText = q4_it1_q5_it2_question8 ^. text
    , _addValueQuestionEventRequiredLevel = q4_it1_q5_it2_question8 ^. requiredLevel
    , _addValueQuestionEventTagUuids = q4_it1_q5_it2_question8 ^. tagUuids
    , _addValueQuestionEventValueType = q4_it1_q5_it2_question8 ^. valueType
    }

e_km1_ch2_q4_it1_q5' :: EditQuestionEvent
e_km1_ch2_q4_it1_q5' = EditListQuestionEvent' e_km1_ch2_q4_it1_q5

e_km1_ch2_q4_it1_q5 :: EditListQuestionEvent
e_km1_ch2_q4_it1_q5 =
  EditListQuestionEvent
    { _editListQuestionEventUuid = fromJust $ U.fromString "17f8e9d4-7299-4c88-aba1-0a7b133aa8f3"
    , _editListQuestionEventParentUuid = question4 ^. uuid
    , _editListQuestionEventEntityUuid = q4_it1_question5Edited ^. uuid
    , _editListQuestionEventTitle = ChangedValue $ q4_it1_question5Edited ^. title
    , _editListQuestionEventText = ChangedValue $ q4_it1_question5Edited ^. text
    , _editListQuestionEventRequiredLevel = ChangedValue $ q4_it1_question5Edited ^. requiredLevel
    , _editListQuestionEventTagUuids = ChangedValue $ q4_it1_question5Edited ^. tagUuids
    , _editListQuestionEventExpertUuids = NothingChanged
    , _editListQuestionEventReferenceUuids = NothingChanged
    , _editListQuestionEventItemTemplateQuestionUuids =
        ChangedValue $ [q4_it1_q5_it2_question8 ^. uuid, q4_it1_q5_it2_question7 ^. uuid]
    }

e_km1_ch2_q4_it1_q6' :: EditQuestionEvent
e_km1_ch2_q4_it1_q6' = EditOptionsQuestionEvent' e_km1_ch2_q4_it1_q6

e_km1_ch2_q4_it1_q6 :: EditOptionsQuestionEvent
e_km1_ch2_q4_it1_q6 =
  EditOptionsQuestionEvent
    { _editOptionsQuestionEventUuid = fromJust $ U.fromString "f5c5ccfd-619b-4110-807a-39ede6d31cae"
    , _editOptionsQuestionEventParentUuid = question4 ^. uuid
    , _editOptionsQuestionEventEntityUuid = q4_it1_question6Edited ^. uuid
    , _editOptionsQuestionEventTitle = ChangedValue $ q4_it1_question6Edited ^. title
    , _editOptionsQuestionEventText = ChangedValue $ q4_it1_question6Edited ^. text
    , _editOptionsQuestionEventRequiredLevel = ChangedValue $ q4_it1_question6Edited ^. requiredLevel
    , _editOptionsQuestionEventTagUuids = ChangedValue $ q4_it1_question6Edited ^. tagUuids
    , _editOptionsQuestionEventExpertUuids = ChangedValue $ q4_it1_question6Edited' ^. expertUuids'
    , _editOptionsQuestionEventReferenceUuids = ChangedValue $ q4_it1_question6Edited' ^. referenceUuids'
    , _editOptionsQuestionEventAnswerUuids = ChangedValue $ q4_it1_question6Edited' ^. answerUuids'
    }

d_km1_ch2_q4_it1_q5 :: DeleteQuestionEvent
d_km1_ch2_q4_it1_q5 =
  DeleteQuestionEvent
    { _deleteQuestionEventUuid = fromJust $ U.fromString "424d19cb-a79f-4da0-b7f6-33363c32b7fd"
    , _deleteQuestionEventParentUuid = question4 ^. uuid
    , _deleteQuestionEventEntityUuid = q4_it1_question5 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- FollowUpQuestionEvent
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_ansYes1_fuq1' :: AddQuestionEvent
a_km1_ch1_ansYes1_fuq1' = AddOptionsQuestionEvent' a_km1_ch1_ansYes1_fuq1

a_km1_ch1_ansYes1_fuq1 :: AddOptionsQuestionEvent
a_km1_ch1_ansYes1_fuq1 =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = fromJust $ U.fromString "3588358c-159e-41a9-9847-262611007b61"
    , _addOptionsQuestionEventParentUuid = q2_answerYes ^. uuid
    , _addOptionsQuestionEventEntityUuid = q2_aYes_fuQuestion1 ^. uuid
    , _addOptionsQuestionEventTitle = q2_aYes_fuQuestion1 ^. title
    , _addOptionsQuestionEventText = q2_aYes_fuQuestion1 ^. text
    , _addOptionsQuestionEventRequiredLevel = q2_aYes_fuQuestion1 ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = q2_aYes_fuQuestion1 ^. tagUuids
    }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2' :: AddQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2' = AddOptionsQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 :: AddOptionsQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = fromJust $ U.fromString "8ced5634-a879-4da2-b7c9-158ca6a4e0e3"
    , _addOptionsQuestionEventParentUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _addOptionsQuestionEventEntityUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _addOptionsQuestionEventTitle = q2_aYes_fuq1_aYes_fuQuestion2 ^. title
    , _addOptionsQuestionEventText = q2_aYes_fuq1_aYes_fuQuestion2 ^. text
    , _addOptionsQuestionEventRequiredLevel = q2_aYes_fuq1_aYes_fuQuestion2 ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = q2_aYes_fuq1_aYes_fuQuestion2 ^. tagUuids
    }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3' :: AddQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3' =
  AddOptionsQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 :: AddOptionsQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 =
  AddOptionsQuestionEvent
    { _addOptionsQuestionEventUuid = fromJust $ U.fromString "6e9b591f-e6f9-46dd-85e8-a90fe4acc51c"
    , _addOptionsQuestionEventParentUuid = q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid
    , _addOptionsQuestionEventEntityUuid = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. uuid
    , _addOptionsQuestionEventTitle = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. title
    , _addOptionsQuestionEventText = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. text
    , _addOptionsQuestionEventRequiredLevel = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. requiredLevel
    , _addOptionsQuestionEventTagUuids = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. tagUuids
    }

a_km1_ch2_ansYes6_fuq4' :: AddQuestionEvent
a_km1_ch2_ansYes6_fuq4' = AddListQuestionEvent' a_km1_ch2_ansYes6_fuq4

a_km1_ch2_ansYes6_fuq4 :: AddListQuestionEvent
a_km1_ch2_ansYes6_fuq4 =
  AddListQuestionEvent
    { _addListQuestionEventUuid = fromJust $ U.fromString "c626fd42-80b8-4fd2-a16b-d38eeb8262f1"
    , _addListQuestionEventParentUuid = q4_it1_q6_answerYes ^. uuid
    , _addListQuestionEventEntityUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _addListQuestionEventTitle = q4_it1_q6_aYes_followUpQuestion4 ^. title
    , _addListQuestionEventText = q4_it1_q6_aYes_followUpQuestion4 ^. text
    , _addListQuestionEventRequiredLevel = q4_it1_q6_aYes_followUpQuestion4 ^. requiredLevel
    , _addListQuestionEventTagUuids = q4_it1_q6_aYes_followUpQuestion4 ^. tagUuids
    }

a_km1_ch2_ansYes6_fuq5' :: AddQuestionEvent
a_km1_ch2_ansYes6_fuq5' = AddIntegrationQuestionEvent' a_km1_ch2_ansYes6_fuq5

a_km1_ch2_ansYes6_fuq5 :: AddIntegrationQuestionEvent
a_km1_ch2_ansYes6_fuq5 =
  AddIntegrationQuestionEvent
    { _addIntegrationQuestionEventUuid = fromJust $ U.fromString "11872ad2-0d3d-4ab6-b81c-17d234bab6ba"
    , _addIntegrationQuestionEventParentUuid = q4_it1_q6_answerYes ^. uuid
    , _addIntegrationQuestionEventEntityUuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
    , _addIntegrationQuestionEventTitle = q4_it1_q6_aYes_followUpQuestion5 ^. title
    , _addIntegrationQuestionEventText = q4_it1_q6_aYes_followUpQuestion5 ^. text
    , _addIntegrationQuestionEventRequiredLevel = q4_it1_q6_aYes_followUpQuestion5 ^. requiredLevel
    , _addIntegrationQuestionEventTagUuids = q4_it1_q6_aYes_followUpQuestion5 ^. tagUuids
    , _addIntegrationQuestionEventIntegrationUuid = q4_it1_q6_aYes_followUpQuestion5 ^. integrationUuid
    , _addIntegrationQuestionEventProps = q4_it1_q6_aYes_followUpQuestion5 ^. props
    }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2' :: EditQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2' = EditOptionsQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: EditOptionsQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  EditOptionsQuestionEvent
    { _editOptionsQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , _editOptionsQuestionEventParentUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _editOptionsQuestionEventEntityUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _editOptionsQuestionEventTitle = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. title
    , _editOptionsQuestionEventText = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. text
    , _editOptionsQuestionEventRequiredLevel = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. requiredLevel
    , _editOptionsQuestionEventTagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. tagUuids
    , _editOptionsQuestionEventExpertUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited' ^. expertUuids'
    , _editOptionsQuestionEventReferenceUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited' ^. referenceUuids'
    , _editOptionsQuestionEventAnswerUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited' ^. answerUuids'
    }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2' :: EditQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2' = EditOptionsQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 :: EditOptionsQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 =
  EditOptionsQuestionEvent
    { _editOptionsQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , _editOptionsQuestionEventParentUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _editOptionsQuestionEventEntityUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    , _editOptionsQuestionEventTitle = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. title
    , _editOptionsQuestionEventText = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. text
    , _editOptionsQuestionEventRequiredLevel = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. requiredLevel
    , _editOptionsQuestionEventTagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. tagUuids
    , _editOptionsQuestionEventExpertUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2' ^. expertUuids'
    , _editOptionsQuestionEventReferenceUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2' ^. referenceUuids'
    , _editOptionsQuestionEventAnswerUuids =
        ChangedValue $ [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
    }

e_km1_ch2_ansMaybe6_fuq4' :: EditQuestionEvent
e_km1_ch2_ansMaybe6_fuq4' = EditListQuestionEvent' e_km1_ch2_ansMaybe6_fuq4

e_km1_ch2_ansMaybe6_fuq4 :: EditListQuestionEvent
e_km1_ch2_ansMaybe6_fuq4 =
  EditListQuestionEvent
    { _editListQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , _editListQuestionEventParentUuid = q4_it1_q6_answerNo ^. uuid
    , _editListQuestionEventEntityUuid = q4_it1_q6_aYes_followUpQuestion4Edited ^. uuid
    , _editListQuestionEventTitle = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. title
    , _editListQuestionEventText = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. text
    , _editListQuestionEventRequiredLevel = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. requiredLevel
    , _editListQuestionEventTagUuids = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. tagUuids
    , _editListQuestionEventExpertUuids = NothingChanged
    , _editListQuestionEventReferenceUuids = NothingChanged
    , _editListQuestionEventItemTemplateQuestionUuids =
        ChangedValue $ [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid, q4_it1_q6_aYes_fuq4_it_question1 ^. uuid]
    }

d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: DeleteQuestionEvent
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  DeleteQuestionEvent
    { _deleteQuestionEventUuid = fromJust $ U.fromString "db69d694-cfb6-4461-8a13-81c01638f348"
    , _deleteQuestionEventParentUuid = q2_aYes_fuq1_answerYes ^. uuid
    , _deleteQuestionEventEntityUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_eAlbert :: AddExpertEvent
a_km1_ch1_q2_eAlbert =
  AddExpertEvent
    { _addExpertEventUuid = fromJust $ U.fromString "ec76054f-d059-4a5f-81c9-1817004a913c"
    , _addExpertEventParentUuid = question2 ^. uuid
    , _addExpertEventEntityUuid = km1_ch1_q2_eAlbert ^. uuid
    , _addExpertEventName = km1_ch1_q2_eAlbert ^. name
    , _addExpertEventEmail = km1_ch1_q2_eAlbert ^. email
    }

a_km1_ch2_q6_eAlbert :: AddExpertEvent
a_km1_ch2_q6_eAlbert =
  AddExpertEvent
    { _addExpertEventUuid = fromJust $ U.fromString "eb6bb073-ecba-4cd0-91a3-ff31d374601f"
    , _addExpertEventParentUuid = q4_it1_question6 ^. uuid
    , _addExpertEventEntityUuid = km1_ch2_q6_eAlbert ^. uuid
    , _addExpertEventName = km1_ch2_q6_eAlbert ^. name
    , _addExpertEventEmail = km1_ch2_q6_eAlbert ^. email
    }

a_km1_ch1_q2_eNikola :: AddExpertEvent
a_km1_ch1_q2_eNikola =
  AddExpertEvent
    { _addExpertEventUuid = fromJust $ U.fromString "40bb45bd-4195-4430-ac8f-16ac5a61ece0"
    , _addExpertEventParentUuid = question2 ^. uuid
    , _addExpertEventEntityUuid = km1_ch1_q2_eNikola ^. uuid
    , _addExpertEventName = km1_ch1_q2_eNikola ^. name
    , _addExpertEventEmail = km1_ch1_q2_eNikola ^. email
    }

a_km1_ch2_q6_eNikola :: AddExpertEvent
a_km1_ch2_q6_eNikola =
  AddExpertEvent
    { _addExpertEventUuid = fromJust $ U.fromString "53653d05-6d5a-4b76-bbc6-15ca8314ad69"
    , _addExpertEventParentUuid = q4_it1_question6 ^. uuid
    , _addExpertEventEntityUuid = km1_ch2_q6_eNikola ^. uuid
    , _addExpertEventName = km1_ch2_q6_eNikola ^. name
    , _addExpertEventEmail = km1_ch2_q6_eNikola ^. email
    }

a_km1_ch1_q2_eIsaac :: AddExpertEvent
a_km1_ch1_q2_eIsaac =
  AddExpertEvent
    { _addExpertEventUuid = fromJust $ U.fromString "2d5eedae-1782-44ac-9d4e-3db769161448"
    , _addExpertEventParentUuid = question2 ^. uuid
    , _addExpertEventEntityUuid = km1_ch1_q2_eIsaac ^. uuid
    , _addExpertEventName = km1_ch1_q2_eIsaac ^. name
    , _addExpertEventEmail = km1_ch1_q2_eIsaac ^. email
    }

e_km1_ch1_q2_eAlbert :: EditExpertEvent
e_km1_ch1_q2_eAlbert =
  EditExpertEvent
    { _editExpertEventUuid = fromJust $ U.fromString "01686131-2423-4d97-a949-4fea2c9ce3b7"
    , _editExpertEventParentUuid = question2 ^. uuid
    , _editExpertEventEntityUuid = km1_ch1_q2_eAlbertEdited ^. uuid
    , _editExpertEventName = ChangedValue $ km1_ch1_q2_eAlbertEdited ^. name
    , _editExpertEventEmail = ChangedValue $ km1_ch1_q2_eAlbertEdited ^. email
    }

d_km1_ch1_q2_eNikola :: DeleteExpertEvent
d_km1_ch1_q2_eNikola =
  DeleteExpertEvent
    { _deleteExpertEventUuid = fromJust $ U.fromString "f20bc988-6d44-4051-990d-d16b24f369ac"
    , _deleteExpertEventParentUuid = question2 ^. uuid
    , _deleteExpertEventEntityUuid = km1_ch1_q2_eNikola ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_rCh1' :: AddReferenceEvent
a_km1_ch1_q2_rCh1' = AddResourcePageReferenceEvent' a_km1_ch1_q2_rCh1

a_km1_ch1_q2_rCh1 :: AddResourcePageReferenceEvent
a_km1_ch1_q2_rCh1 =
  AddResourcePageReferenceEvent
    { _addResourcePageReferenceEventUuid = fromJust $ U.fromString "1177d72f-b7d8-466d-ad33-d5f82d0f192a"
    , _addResourcePageReferenceEventParentUuid = question2 ^. uuid
    , _addResourcePageReferenceEventEntityUuid = km1_ch1_q2_r1 ^. uuid
    , _addResourcePageReferenceEventShortUuid = km1_ch1_q2_r1 ^. shortUuid
    }

a_km1_ch2_q6_rCh1' :: AddReferenceEvent
a_km1_ch2_q6_rCh1' = AddResourcePageReferenceEvent' a_km1_ch2_q6_rCh1

a_km1_ch2_q6_rCh1 :: AddResourcePageReferenceEvent
a_km1_ch2_q6_rCh1 =
  AddResourcePageReferenceEvent
    { _addResourcePageReferenceEventUuid = fromJust $ U.fromString "a3f6ee9a-803f-4911-9566-734a6358913a"
    , _addResourcePageReferenceEventParentUuid = q4_it1_question6 ^. uuid
    , _addResourcePageReferenceEventEntityUuid = km1_ch2_q6_r1 ^. uuid
    , _addResourcePageReferenceEventShortUuid = km1_ch2_q6_r1 ^. shortUuid
    }

a_km1_ch1_q2_rCh2' :: AddReferenceEvent
a_km1_ch1_q2_rCh2' = AddURLReferenceEvent' a_km1_ch1_q2_rCh2

a_km1_ch1_q2_rCh2 :: AddURLReferenceEvent
a_km1_ch1_q2_rCh2 =
  AddURLReferenceEvent
    { _addURLReferenceEventUuid = fromJust $ U.fromString "4814f50f-8838-4b53-8b18-c0f8c568220e"
    , _addURLReferenceEventParentUuid = question2 ^. uuid
    , _addURLReferenceEventEntityUuid = km1_ch1_q2_r2 ^. uuid
    , _addURLReferenceEventUrl = km1_ch1_q2_r2 ^. url
    , _addURLReferenceEventLabel = km1_ch1_q2_r2 ^. label
    }

a_km1_ch2_q6_rCh2' :: AddReferenceEvent
a_km1_ch2_q6_rCh2' = AddURLReferenceEvent' a_km1_ch2_q6_rCh2

a_km1_ch2_q6_rCh2 :: AddURLReferenceEvent
a_km1_ch2_q6_rCh2 =
  AddURLReferenceEvent
    { _addURLReferenceEventUuid = fromJust $ U.fromString "a4ae3400-dd3c-41ab-b796-4bf9d0bdafe7"
    , _addURLReferenceEventParentUuid = q4_it1_question6 ^. uuid
    , _addURLReferenceEventEntityUuid = km1_ch2_q6_r2 ^. uuid
    , _addURLReferenceEventUrl = km1_ch2_q6_r2 ^. url
    , _addURLReferenceEventLabel = km1_ch2_q6_r2 ^. label
    }

a_km1_ch1_q2_rCh3' :: AddReferenceEvent
a_km1_ch1_q2_rCh3' = AddCrossReferenceEvent' a_km1_ch1_q2_rCh3

a_km1_ch1_q2_rCh3 :: AddCrossReferenceEvent
a_km1_ch1_q2_rCh3 =
  AddCrossReferenceEvent
    { _addCrossReferenceEventUuid = fromJust $ U.fromString "45d8ec86-34bc-4e8f-b42a-48a567a77d8b"
    , _addCrossReferenceEventParentUuid = question2 ^. uuid
    , _addCrossReferenceEventEntityUuid = km1_ch1_q2_r3 ^. uuid
    , _addCrossReferenceEventTargetUuid = km1_ch1_q2_r3 ^. targetUuid
    , _addCrossReferenceEventDescription = km1_ch1_q2_r3 ^. description
    }

e_km1_ch1_q2_rCh1' :: EditReferenceEvent
e_km1_ch1_q2_rCh1' = EditResourcePageReferenceEvent' e_km1_ch1_q2_rCh1

e_km1_ch1_q2_rCh1 :: EditResourcePageReferenceEvent
e_km1_ch1_q2_rCh1 =
  EditResourcePageReferenceEvent
    { _editResourcePageReferenceEventUuid = fromJust $ U.fromString "08cd9afc-d416-48ab-8669-17e87ceb15dc"
    , _editResourcePageReferenceEventParentUuid = question2 ^. uuid
    , _editResourcePageReferenceEventEntityUuid = km1_ch1_q2_r1Edited ^. uuid
    , _editResourcePageReferenceEventShortUuid = ChangedValue $ km1_ch1_q2_r1Edited ^. shortUuid
    }

e_km1_ch1_q2_rCh1_type' :: EditReferenceEvent
e_km1_ch1_q2_rCh1_type' = EditURLReferenceEvent' e_km1_ch1_q2_rCh1_type

e_km1_ch1_q2_rCh1_type :: EditURLReferenceEvent
e_km1_ch1_q2_rCh1_type =
  EditURLReferenceEvent
    { _editURLReferenceEventUuid = fromJust $ U.fromString "4e1058cf-9044-42a0-901c-816bd6847b17"
    , _editURLReferenceEventParentUuid = question2 ^. uuid
    , _editURLReferenceEventEntityUuid = km1_ch1_q2_r1WithNewType ^. uuid
    , _editURLReferenceEventUrl = ChangedValue $ km1_ch1_q2_r1WithNewType ^. url
    , _editURLReferenceEventLabel = ChangedValue $ km1_ch1_q2_r1WithNewType ^. label
    }

e_km1_ch1_q2_rCh2' :: EditReferenceEvent
e_km1_ch1_q2_rCh2' = EditURLReferenceEvent' e_km1_ch1_q2_rCh2

e_km1_ch1_q2_rCh2 :: EditURLReferenceEvent
e_km1_ch1_q2_rCh2 =
  EditURLReferenceEvent
    { _editURLReferenceEventUuid = fromJust $ U.fromString "f96588ae-1657-406e-9810-1d00f5e24a96"
    , _editURLReferenceEventParentUuid = question2 ^. uuid
    , _editURLReferenceEventEntityUuid = km1_ch1_q2_r2Edited ^. uuid
    , _editURLReferenceEventUrl = ChangedValue $ km1_ch1_q2_r2Edited ^. url
    , _editURLReferenceEventLabel = ChangedValue $ km1_ch1_q2_r2Edited ^. label
    }

e_km1_ch1_q2_rCh2_type' :: EditReferenceEvent
e_km1_ch1_q2_rCh2_type' = EditCrossReferenceEvent' e_km1_ch1_q2_rCh2_type

e_km1_ch1_q2_rCh2_type :: EditCrossReferenceEvent
e_km1_ch1_q2_rCh2_type =
  EditCrossReferenceEvent
    { _editCrossReferenceEventUuid = fromJust $ U.fromString "e0a19e9d-fb36-47b3-bc23-f752f7403937"
    , _editCrossReferenceEventParentUuid = question2 ^. uuid
    , _editCrossReferenceEventEntityUuid = km1_ch1_q2_r2WithNewType ^. uuid
    , _editCrossReferenceEventTargetUuid = ChangedValue $ km1_ch1_q2_r2WithNewType ^. targetUuid
    , _editCrossReferenceEventDescription = ChangedValue $ km1_ch1_q2_r2WithNewType ^. description
    }

e_km1_ch1_q2_rCh3' :: EditReferenceEvent
e_km1_ch1_q2_rCh3' = EditCrossReferenceEvent' e_km1_ch1_q2_rCh3

e_km1_ch1_q2_rCh3 :: EditCrossReferenceEvent
e_km1_ch1_q2_rCh3 =
  EditCrossReferenceEvent
    { _editCrossReferenceEventUuid = fromJust $ U.fromString "d3a7b6a6-9e87-4308-a103-88245537c26e"
    , _editCrossReferenceEventParentUuid = question2 ^. uuid
    , _editCrossReferenceEventEntityUuid = km1_ch1_q2_r3Edited ^. uuid
    , _editCrossReferenceEventTargetUuid = ChangedValue $ km1_ch1_q2_r3Edited ^. targetUuid
    , _editCrossReferenceEventDescription = ChangedValue $ km1_ch1_q2_r3Edited ^. description
    }

e_km1_ch1_q2_rCh3_type' :: EditReferenceEvent
e_km1_ch1_q2_rCh3_type' = EditResourcePageReferenceEvent' e_km1_ch1_q2_rCh3_type

e_km1_ch1_q2_rCh3_type :: EditResourcePageReferenceEvent
e_km1_ch1_q2_rCh3_type =
  EditResourcePageReferenceEvent
    { _editResourcePageReferenceEventUuid = fromJust $ U.fromString "f8528e3b-4904-4ad8-87b8-809d7e40c087"
    , _editResourcePageReferenceEventParentUuid = question2 ^. uuid
    , _editResourcePageReferenceEventEntityUuid = km1_ch1_q2_r3WithNewType ^. uuid
    , _editResourcePageReferenceEventShortUuid = ChangedValue $ km1_ch1_q2_r3WithNewType ^. shortUuid
    }

d_km1_ch1_q2_rCh2 :: DeleteReferenceEvent
d_km1_ch1_q2_rCh2 =
  DeleteReferenceEvent
    { _deleteReferenceEventUuid = fromJust $ U.fromString "3cc15f31-4801-404f-ba48-6b91f77d1abe"
    , _deleteReferenceEventParentUuid = question2 ^. uuid
    , _deleteReferenceEventEntityUuid = km1_ch1_q2_r2 ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_tds :: AddTagEvent
a_km1_tds =
  AddTagEvent
    { _addTagEventUuid = fromJust $ U.fromString "dedc4a9d-00d9-41b6-8494-a10a238be03b"
    , _addTagEventParentUuid = km1 ^. uuid
    , _addTagEventEntityUuid = tagDataScience ^. uuid
    , _addTagEventName = tagDataScience ^. name
    , _addTagEventDescription = tagDataScience ^. description
    , _addTagEventColor = tagDataScience ^. color
    }

a_km1_tbi :: AddTagEvent
a_km1_tbi =
  AddTagEvent
    { _addTagEventUuid = fromJust $ U.fromString "b6b0e53c-5702-403c-950c-e04960e09e73"
    , _addTagEventParentUuid = km1 ^. uuid
    , _addTagEventEntityUuid = tagBioInformatic ^. uuid
    , _addTagEventName = tagBioInformatic ^. name
    , _addTagEventDescription = tagBioInformatic ^. description
    , _addTagEventColor = tagBioInformatic ^. color
    }

e_km1_tds :: EditTagEvent
e_km1_tds =
  EditTagEvent
    { _editTagEventUuid = fromJust $ U.fromString "f68f764b-48d1-4b30-8d53-48cfa2752801"
    , _editTagEventParentUuid = km1 ^. uuid
    , _editTagEventEntityUuid = tagDataScienceEdited ^. uuid
    , _editTagEventName = ChangedValue $ tagDataScienceEdited ^. name
    , _editTagEventDescription = ChangedValue $ tagDataScienceEdited ^. description
    , _editTagEventColor = ChangedValue $ tagDataScienceEdited ^. color
    }

d_km1_tds :: DeleteTagEvent
d_km1_tds =
  DeleteTagEvent
    { _deleteTagEventUuid = fromJust $ U.fromString "969d00c2-062d-4763-a372-536d486c532f"
    , _deleteTagEventParentUuid = km1 ^. uuid
    , _deleteTagEventEntityUuid = tagDataScience ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_iop :: AddIntegrationEvent
a_km1_iop =
  AddIntegrationEvent
    { _addIntegrationEventUuid = fromJust $ U.fromString "3f94cb01-6f92-4eb6-975b-385c02b831bc"
    , _addIntegrationEventParentUuid = km1 ^. uuid
    , _addIntegrationEventEntityUuid = ontologyPortal ^. uuid
    , _addIntegrationEventIId = ontologyPortal ^. iId
    , _addIntegrationEventName = ontologyPortal ^. name
    , _addIntegrationEventProps = ontologyPortal ^. props
    , _addIntegrationEventLogo = ontologyPortal ^. logo
    , _addIntegrationEventRequestMethod = ontologyPortal ^. requestMethod
    , _addIntegrationEventRequestUrl = ontologyPortal ^. requestUrl
    , _addIntegrationEventRequestHeaders = ontologyPortal ^. requestHeaders
    , _addIntegrationEventRequestBody = ontologyPortal ^. requestBody
    , _addIntegrationEventResponseListField = ontologyPortal ^. responseListField
    , _addIntegrationEventResponseIdField = ontologyPortal ^. responseIdField
    , _addIntegrationEventResponseNameField = ontologyPortal ^. responseNameField
    , _addIntegrationEventItemUrl = ontologyPortal ^. itemUrl
    }

a_km1_ibp :: AddIntegrationEvent
a_km1_ibp =
  AddIntegrationEvent
    { _addIntegrationEventUuid = fromJust $ U.fromString "5c47b31c-84d0-4792-99ce-09154642105d"
    , _addIntegrationEventParentUuid = km1 ^. uuid
    , _addIntegrationEventEntityUuid = bioPortal ^. uuid
    , _addIntegrationEventIId = bioPortal ^. iId
    , _addIntegrationEventName = bioPortal ^. name
    , _addIntegrationEventProps = bioPortal ^. props
    , _addIntegrationEventLogo = bioPortal ^. logo
    , _addIntegrationEventRequestMethod = bioPortal ^. requestMethod
    , _addIntegrationEventRequestUrl = bioPortal ^. requestUrl
    , _addIntegrationEventRequestHeaders = bioPortal ^. requestHeaders
    , _addIntegrationEventRequestBody = bioPortal ^. requestBody
    , _addIntegrationEventResponseListField = bioPortal ^. responseListField
    , _addIntegrationEventResponseIdField = bioPortal ^. responseIdField
    , _addIntegrationEventResponseNameField = bioPortal ^. responseNameField
    , _addIntegrationEventItemUrl = bioPortal ^. itemUrl
    }

e_km1_iop :: EditIntegrationEvent
e_km1_iop =
  EditIntegrationEvent
    { _editIntegrationEventUuid = fromJust $ U.fromString "3456a254-c5bc-4c0e-8ff9-f5e080765a71"
    , _editIntegrationEventParentUuid = km1 ^. uuid
    , _editIntegrationEventEntityUuid = ontologyPortalEdited ^. uuid
    , _editIntegrationEventIId = ChangedValue $ ontologyPortalEdited ^. iId
    , _editIntegrationEventName = ChangedValue $ ontologyPortalEdited ^. name
    , _editIntegrationEventProps = ChangedValue $ ontologyPortalEdited ^. props
    , _editIntegrationEventLogo = ChangedValue $ ontologyPortalEdited ^. logo
    , _editIntegrationEventRequestMethod = ChangedValue $ ontologyPortalEdited ^. requestMethod
    , _editIntegrationEventRequestUrl = ChangedValue $ ontologyPortalEdited ^. requestUrl
    , _editIntegrationEventRequestHeaders = ChangedValue $ ontologyPortalEdited ^. requestHeaders
    , _editIntegrationEventRequestBody = ChangedValue $ ontologyPortalEdited ^. requestBody
    , _editIntegrationEventResponseListField = ChangedValue $ ontologyPortalEdited ^. responseListField
    , _editIntegrationEventResponseIdField = ChangedValue $ ontologyPortalEdited ^. responseIdField
    , _editIntegrationEventResponseNameField = ChangedValue $ ontologyPortalEdited ^. responseNameField
    , _editIntegrationEventItemUrl = ChangedValue $ ontologyPortalEdited ^. itemUrl
    }

d_km1_iop :: DeleteIntegrationEvent
d_km1_iop =
  DeleteIntegrationEvent
    { _deleteIntegrationEventUuid = fromJust $ U.fromString "d211d46f-5358-497a-92a0-e0bde08ce3d3"
    , _deleteIntegrationEventParentUuid = km1 ^. uuid
    , _deleteIntegrationEventEntityUuid = ontologyPortal ^. uuid
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
m_km1_ch1_q1__to_ch2 :: MoveQuestionEvent
m_km1_ch1_q1__to_ch2 =
  MoveQuestionEvent
    { _moveQuestionEventUuid = fromJust $ U.fromString "f13a1d1b-5cb6-458a-ad99-cafe3912aa1d"
    , _moveQuestionEventParentUuid = chapter1 ^. uuid
    , _moveQuestionEventEntityUuid = question1 ^. uuid
    , _moveQuestionEventTargetUuid = chapter2 ^. uuid
    }

m_km1_ch1_q1__to_ch2_q3_aNo :: MoveQuestionEvent
m_km1_ch1_q1__to_ch2_q3_aNo =
  MoveQuestionEvent
    { _moveQuestionEventUuid = fromJust $ U.fromString "3bf501a1-cbc2-4b94-9b17-d23f0bad7fc9"
    , _moveQuestionEventParentUuid = chapter1 ^. uuid
    , _moveQuestionEventEntityUuid = question1 ^. uuid
    , _moveQuestionEventTargetUuid = q3_answerNo ^. uuid
    }

m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo :: MoveQuestionEvent
m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo =
  MoveQuestionEvent
    { _moveQuestionEventUuid = fromJust $ U.fromString "a2f35e98-dd67-45cf-a18e-a8a38382c7be"
    , _moveQuestionEventParentUuid = question4 ^. uuid
    , _moveQuestionEventEntityUuid = q4_it1_question5 ^. uuid
    , _moveQuestionEventTargetUuid = q4_it1_q6_answerNo ^. uuid
    }

m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4 :: MoveQuestionEvent
m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4 =
  MoveQuestionEvent
    { _moveQuestionEventUuid = fromJust $ U.fromString "a2f35e98-dd67-45cf-a18e-a8a38382c7be"
    , _moveQuestionEventParentUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
    , _moveQuestionEventEntityUuid = q4_it1_q6_aYes_fuq4_it_question1 ^. uuid
    , _moveQuestionEventTargetUuid = question4 ^. uuid
    }

m_km1_ch1_q2_aYes__to_ch2_q3 :: MoveAnswerEvent
m_km1_ch1_q2_aYes__to_ch2_q3 =
  MoveAnswerEvent
    { _moveAnswerEventUuid = fromJust $ U.fromString "b660447a-ddbd-482a-9610-68dfca6a25fd"
    , _moveAnswerEventParentUuid = question2 ^. uuid
    , _moveAnswerEventEntityUuid = q2_answerYes ^. uuid
    , _moveAnswerEventTargetUuid = question3 ^. uuid
    }

m_km1_ch1_q2_eAlbert__to_ch2_q3 :: MoveExpertEvent
m_km1_ch1_q2_eAlbert__to_ch2_q3 =
  MoveExpertEvent
    { _moveExpertEventUuid = fromJust $ U.fromString "35b18cb0-912f-4c76-9f80-b6bfc6479c7c"
    , _moveExpertEventParentUuid = question2 ^. uuid
    , _moveExpertEventEntityUuid = km1_ch1_q2_eAlbert ^. uuid
    , _moveExpertEventTargetUuid = question3 ^. uuid
    }

m_km1_ch1_q2_r1__to_ch2_q3 :: MoveReferenceEvent
m_km1_ch1_q2_r1__to_ch2_q3 =
  MoveReferenceEvent
    { _moveReferenceEventUuid = fromJust $ U.fromString "1cc9ad2b-22bc-4806-902e-49b46ccc14d5"
    , _moveReferenceEventParentUuid = question2 ^. uuid
    , _moveReferenceEventEntityUuid = km1_ch1_q2_r1 ^. uuid
    , _moveReferenceEventTargetUuid = question3 ^. uuid
    }
