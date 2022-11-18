module Shared.Database.Migration.Development.Event.Data.Events where

import qualified Data.UUID as U

import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Choices
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Metrics
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Model.Event.Answer.AnswerEvent
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.Event.EventField
import Shared.Model.Event.Expert.ExpertEvent
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.Event.Metric.MetricEvent
import Shared.Model.Event.Move.MoveEvent
import Shared.Model.Event.Phase.PhaseEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Date
import Shared.Util.Uuid

a_km1 :: AddKnowledgeModelEvent
a_km1 =
  AddKnowledgeModelEvent
    { uuid = u' "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f"
    , parentUuid = U.nil
    , entityUuid = km1WithoutChaptersAndTagsAndIntegrations.uuid
    , annotations = km1WithoutChaptersAndTagsAndIntegrations.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1 :: EditKnowledgeModelEvent
e_km1 =
  EditKnowledgeModelEvent
    { uuid = u' "8294a55d-642d-416c-879b-5a42a4430c24"
    , parentUuid = U.nil
    , entityUuid = km1.uuid
    , annotations = ChangedValue $ km1Edited.annotations
    , chapterUuids = ChangedValue $ km1Edited.chapterUuids
    , tagUuids = ChangedValue $ km1Edited.tagUuids
    , integrationUuids = ChangedValue $ km1Edited.integrationUuids
    , metricUuids = ChangedValue $ km1Edited.metricUuids
    , phaseUuids = ChangedValue $ km1Edited.phaseUuids
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1 :: AddChapterEvent
a_km1_ch1 =
  AddChapterEvent
    { uuid = u' "dedc4a9d-00d9-41b6-8494-a10a238be03b"
    , parentUuid = km1.uuid
    , entityUuid = chapter1WithoutQuestions.uuid
    , title = chapter1WithoutQuestions.title
    , text = chapter1WithoutQuestions.text
    , annotations = chapter1WithoutQuestions.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2 :: AddChapterEvent
a_km1_ch2 =
  AddChapterEvent
    { uuid = u' "6c4bba6e-864b-4871-98ca-49ac7a3e5eb5"
    , parentUuid = km1.uuid
    , entityUuid = chapter2WithoutQuestions.uuid
    , title = chapter2WithoutQuestions.title
    , text = chapter2WithoutQuestions.text
    , annotations = chapter2WithoutQuestions.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch3 :: AddChapterEvent
a_km1_ch3 =
  AddChapterEvent
    { uuid = u' "6eaa2b47-711d-4187-98f8-fccdce94db9b"
    , parentUuid = km1.uuid
    , entityUuid = chapter3.uuid
    , title = chapter3.title
    , text = chapter3.text
    , annotations = chapter3.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch4 :: AddChapterEvent
a_km1_ch4 =
  AddChapterEvent
    { uuid = u' "6585a64d-c75b-47fc-a86e-e0c8e773528f"
    , parentUuid = km1.uuid
    , entityUuid = chapter4WithoutQuestions.uuid
    , title = chapter4WithoutQuestions.title
    , text = chapter4WithoutQuestions.text
    , annotations = chapter4WithoutQuestions.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1 :: EditChapterEvent
e_km1_ch1 =
  EditChapterEvent
    { uuid = u' "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
    , parentUuid = km1.uuid
    , entityUuid = chapter1.uuid
    , title = ChangedValue $ chapter1Edited.title
    , text = ChangedValue $ chapter1Edited.text
    , annotations = ChangedValue $ chapter1Edited.annotations
    , questionUuids = ChangedValue $ chapter1Edited.questionUuids
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_2 :: EditChapterEvent
e_km1_ch1_2 =
  EditChapterEvent
    { uuid = u' "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
    , parentUuid = km1.uuid
    , entityUuid = chapter1.uuid
    , title = ChangedValue $ "TWICE: " ++ chapter1Edited.title
    , text = ChangedValue $ chapter1Edited.text
    , annotations = ChangedValue $ chapter1Edited.annotations
    , questionUuids = ChangedValue $ chapter1Edited.questionUuids
    , createdAt = dt' 2018 1 21
    }

d_km1_ch1 :: DeleteChapterEvent
d_km1_ch1 =
  DeleteChapterEvent
    { uuid = u' "d07cc69b-abd3-43ec-bce1-fe59899dbda3"
    , parentUuid = km1.uuid
    , entityUuid = chapter1.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q1' :: AddQuestionEvent
a_km1_ch1_q1' = AddValueQuestionEvent' a_km1_ch1_q1

a_km1_ch1_q1 :: AddValueQuestionEvent
a_km1_ch1_q1 =
  AddValueQuestionEvent
    { uuid = u' "71ae2ce9-553b-4ca2-a542-1bce04406c51"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , title = question1.title
    , text = question1.text
    , requiredPhaseUuid = question1.requiredPhaseUuid
    , annotations = question1.annotations
    , tagUuids = question1.tagUuids
    , valueType = question1.valueType
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2' :: AddQuestionEvent
a_km1_ch1_q2' = AddOptionsQuestionEvent' a_km1_ch1_q2

a_km1_ch1_q2 :: AddOptionsQuestionEvent
a_km1_ch1_q2 =
  AddOptionsQuestionEvent
    { uuid = u' "ced9be29-24af-4443-8f5f-e709791a8fe3"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , title = question2.title
    , text = question2.text
    , requiredPhaseUuid = question2.requiredPhaseUuid
    , annotations = question2.annotations
    , tagUuids = question2.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q3' :: AddQuestionEvent
a_km1_ch1_q3' = AddOptionsQuestionEvent' a_km1_ch1_q3

a_km1_ch1_q3 :: AddOptionsQuestionEvent
a_km1_ch1_q3 =
  AddOptionsQuestionEvent
    { uuid = u' "d559ac95-cc81-4502-a780-dbaee46f24bc"
    , parentUuid = chapter1.uuid
    , entityUuid = question3.uuid
    , title = question3.title
    , text = question3.text
    , requiredPhaseUuid = question3.requiredPhaseUuid
    , annotations = question3.annotations
    , tagUuids = question3.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q3' :: AddQuestionEvent
a_km1_ch2_q3' = AddOptionsQuestionEvent' a_km1_ch2_q3

a_km1_ch2_q3 :: AddOptionsQuestionEvent
a_km1_ch2_q3 =
  AddOptionsQuestionEvent
    { uuid = u' "bc994b0f-bee1-4f28-9945-9714b0e559e9"
    , parentUuid = chapter2.uuid
    , entityUuid = question3.uuid
    , title = question3.title
    , text = question3.text
    , requiredPhaseUuid = question3.requiredPhaseUuid
    , annotations = question3.annotations
    , tagUuids = question3.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q4' :: AddQuestionEvent
a_km1_ch2_q4' = AddListQuestionEvent' a_km1_ch2_q4

a_km1_ch2_q4 :: AddListQuestionEvent
a_km1_ch2_q4 =
  AddListQuestionEvent
    { uuid = u' "bc994b0f-bee1-4f28-9945-9714b0e559e9"
    , parentUuid = chapter2.uuid
    , entityUuid = question4.uuid
    , title = question4.title
    , text = question4.text
    , requiredPhaseUuid = question4.requiredPhaseUuid
    , annotations = question4.annotations
    , tagUuids = question4.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch3_q9' :: AddQuestionEvent
a_km1_ch3_q9' = AddIntegrationQuestionEvent' a_km1_ch3_q9

a_km1_ch3_q9 :: AddIntegrationQuestionEvent
a_km1_ch3_q9 =
  AddIntegrationQuestionEvent
    { uuid = u' "51526318-2727-4113-993d-bae5d4abafcd"
    , parentUuid = chapter3.uuid
    , entityUuid = question9.uuid
    , title = question9.title
    , text = question9.text
    , requiredPhaseUuid = question9.requiredPhaseUuid
    , annotations = question9.annotations
    , tagUuids = question9.tagUuids
    , integrationUuid = question9.integrationUuid
    , props = question9.props
    , createdAt = dt' 2018 1 21
    }

a_km1_ch3_q10' :: AddQuestionEvent
a_km1_ch3_q10' = AddIntegrationQuestionEvent' a_km1_ch3_q10

a_km1_ch3_q10 :: AddIntegrationQuestionEvent
a_km1_ch3_q10 =
  AddIntegrationQuestionEvent
    { uuid = u' "e8531168-946d-4d95-a3b5-f092d32dee1a"
    , parentUuid = chapter3.uuid
    , entityUuid = question10.uuid
    , title = question10.title
    , text = question10.text
    , requiredPhaseUuid = question10.requiredPhaseUuid
    , tagUuids = question10.tagUuids
    , integrationUuid = question10.integrationUuid
    , annotations = question10.annotations
    , props = question10.props
    , createdAt = dt' 2018 1 21
    }

a_km1_ch3_q11' :: AddQuestionEvent
a_km1_ch3_q11' = AddMultiChoiceQuestionEvent' a_km1_ch3_q11

a_km1_ch3_q11 :: AddMultiChoiceQuestionEvent
a_km1_ch3_q11 =
  AddMultiChoiceQuestionEvent
    { uuid = u' "2083c6d2-6fa4-4170-8b14-5f5a518b78b2"
    , parentUuid = chapter3.uuid
    , entityUuid = question11.uuid
    , title = question11.title
    , text = question11.text
    , requiredPhaseUuid = question11.requiredPhaseUuid
    , annotations = question11.annotations
    , tagUuids = question11.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch3_q12' :: AddQuestionEvent
a_km1_ch3_q12' = AddMultiChoiceQuestionEvent' a_km1_ch3_q12

a_km1_ch3_q12 :: AddMultiChoiceQuestionEvent
a_km1_ch3_q12 =
  AddMultiChoiceQuestionEvent
    { uuid = u' "e5e6eb01-f55f-422b-9423-ada60f55b36c"
    , parentUuid = chapter3.uuid
    , entityUuid = question12.uuid
    , title = question12.title
    , text = question12.text
    , requiredPhaseUuid = question12.requiredPhaseUuid
    , annotations = question12.annotations
    , tagUuids = question12.tagUuids
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q1' :: EditQuestionEvent
e_km1_ch1_q1' = EditValueQuestionEvent' e_km1_ch1_q1

e_km1_ch1_q1 :: EditValueQuestionEvent
e_km1_ch1_q1 =
  EditValueQuestionEvent
    { uuid = u' "de86f82b-aaaf-482e-97c7-c7e93d834cd9"
    , parentUuid = chapter1.uuid
    , entityUuid = question1Edited.uuid
    , title = ChangedValue $ question1Edited.title
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = ChangedValue $ question1Edited.annotations
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , valueType = NothingChanged
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q1_type' :: EditQuestionEvent
e_km1_ch1_q1_type' = EditOptionsQuestionEvent' e_km1_ch1_q1_type

e_km1_ch1_q1_type :: EditOptionsQuestionEvent
e_km1_ch1_q1_type =
  EditOptionsQuestionEvent
    { uuid = u' "f56b1435-ec9f-4d79-88b3-04c39b73724d"
    , parentUuid = chapter1.uuid
    , entityUuid = question1WithNewType.uuid
    , title = ChangedValue $ question1WithNewType.title
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = ChangedValue $ question1WithNewType.annotations
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = ChangedValue $ question1WithNewType.answerUuids
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2' :: EditQuestionEvent
e_km1_ch1_q2' = EditOptionsQuestionEvent' e_km1_ch1_q2

e_km1_ch1_q2 :: EditOptionsQuestionEvent
e_km1_ch1_q2 =
  EditOptionsQuestionEvent
    { uuid = u' "1a01665b-e896-450d-b606-afc1dcca586b"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , title = ChangedValue $ question2Edited.title
    , text = ChangedValue $ question2Edited.text
    , requiredPhaseUuid = ChangedValue $ question2Edited.requiredPhaseUuid
    , annotations = ChangedValue $ question2Edited.annotations
    , tagUuids = ChangedValue $ question2Edited.tagUuids
    , expertUuids = ChangedValue $ question2Edited.expertUuids
    , referenceUuids = ChangedValue $ question2Edited.referenceUuids
    , answerUuids = ChangedValue $ question2Edited.answerUuids
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_second_edit' :: EditQuestionEvent
e_km1_ch1_q2_second_edit' = EditOptionsQuestionEvent' e_km1_ch1_q2_second_edit

e_km1_ch1_q2_second_edit :: EditOptionsQuestionEvent
e_km1_ch1_q2_second_edit =
  EditOptionsQuestionEvent
    { uuid = u' "bf888b95-921d-4caa-88af-3309393d44c3"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , title = ChangedValue "New title"
    , text = ChangedValue $ question2Edited.text
    , requiredPhaseUuid = ChangedValue $ question2Edited.requiredPhaseUuid
    , annotations = ChangedValue $ question2Edited.annotations
    , tagUuids = ChangedValue $ question2Edited.tagUuids
    , expertUuids = ChangedValue $ question2Edited.expertUuids
    , referenceUuids = ChangedValue $ question2Edited.referenceUuids
    , answerUuids = ChangedValue $ question2Edited.answerUuids
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_type' :: EditQuestionEvent
e_km1_ch1_q2_type' = EditListQuestionEvent' e_km1_ch1_q2_type

e_km1_ch1_q2_type :: EditListQuestionEvent
e_km1_ch1_q2_type =
  EditListQuestionEvent
    { uuid = u' "2727c225-78e5-4d5f-a093-cfaadb6ea663"
    , parentUuid = chapter1.uuid
    , entityUuid = question2WithNewType.uuid
    , title = ChangedValue $ question2WithNewType.title
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = ChangedValue $ question2WithNewType.annotations
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , itemTemplateQuestionUuids = ChangedValue []
    , createdAt = dt' 2018 1 21
    }

e_km1_ch2_q4' :: EditQuestionEvent
e_km1_ch2_q4' = EditListQuestionEvent' e_km1_ch2_q4

e_km1_ch2_q4 :: EditListQuestionEvent
e_km1_ch2_q4 =
  EditListQuestionEvent
    { uuid = u' "7014c6de-a1c0-4c09-881a-c83c68a29de1"
    , parentUuid = chapter2.uuid
    , entityUuid = question4Edited.uuid
    , title = ChangedValue $ question4Edited.title
    , text = ChangedValue $ question4Edited.text
    , requiredPhaseUuid = ChangedValue $ question4Edited.requiredPhaseUuid
    , annotations = ChangedValue $ question4Edited.annotations
    , tagUuids = ChangedValue $ question4Edited.tagUuids
    , expertUuids = ChangedValue $ question4Edited.expertUuids
    , referenceUuids = ChangedValue $ question4Edited.referenceUuids
    , itemTemplateQuestionUuids = ChangedValue $ question4Edited.itemTemplateQuestionUuids
    , createdAt = dt' 2018 1 21
    }

e_km1_ch2_q4_type' :: EditQuestionEvent
e_km1_ch2_q4_type' = EditIntegrationQuestionEvent' e_km1_ch2_q4_type

e_km1_ch2_q4_type :: EditIntegrationQuestionEvent
e_km1_ch2_q4_type =
  EditIntegrationQuestionEvent
    { uuid = u' "0f6f536c-aa1c-4d47-8cd7-46d611b43a56"
    , parentUuid = chapter2.uuid
    , entityUuid = question4WithNewType.uuid
    , title = ChangedValue $ question4WithNewType.title
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = ChangedValue $ question4WithNewType.annotations
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , integrationUuid = ChangedValue $ question4WithNewType.integrationUuid
    , props = ChangedValue $ question4WithNewType.props
    , createdAt = dt' 2018 1 21
    }

e_km1_ch3_q9' :: EditQuestionEvent
e_km1_ch3_q9' = EditIntegrationQuestionEvent' e_km1_ch3_q9

e_km1_ch3_q9 :: EditIntegrationQuestionEvent
e_km1_ch3_q9 =
  EditIntegrationQuestionEvent
    { uuid = u' "43779823-507b-41f1-8dce-7c5e0660db8f"
    , parentUuid = chapter3.uuid
    , entityUuid = question9Edited.uuid
    , title = ChangedValue $ question9Edited.title
    , text = ChangedValue $ question9Edited.text
    , requiredPhaseUuid = ChangedValue $ question9Edited.requiredPhaseUuid
    , annotations = ChangedValue $ question9Edited.annotations
    , tagUuids = ChangedValue $ question9Edited.tagUuids
    , expertUuids = ChangedValue $ question9Edited.expertUuids
    , referenceUuids = ChangedValue $ question9Edited.referenceUuids
    , integrationUuid = ChangedValue $ question9Edited.integrationUuid
    , props = ChangedValue $ question9Edited.props
    , createdAt = dt' 2018 1 21
    }

e_km1_ch3_q9_type' :: EditQuestionEvent
e_km1_ch3_q9_type' = EditValueQuestionEvent' e_km1_ch3_q9_type

e_km1_ch3_q9_type :: EditValueQuestionEvent
e_km1_ch3_q9_type =
  EditValueQuestionEvent
    { uuid = u' "91514dc3-29b1-469a-b0d9-5fc211df1c47"
    , parentUuid = chapter3.uuid
    , entityUuid = question9WithNewType.uuid
    , title = ChangedValue $ question9WithNewType.title
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = ChangedValue $ question9WithNewType.annotations
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , valueType = ChangedValue $ question9WithNewType.valueType
    , createdAt = dt' 2018 1 21
    }

e_km1_ch3_q11' :: EditQuestionEvent
e_km1_ch3_q11' = EditMultiChoiceQuestionEvent' e_km1_ch3_q11

e_km1_ch3_q11 :: EditMultiChoiceQuestionEvent
e_km1_ch3_q11 =
  EditMultiChoiceQuestionEvent
    { uuid = u' "1a01665b-e896-450d-b606-afc1dcca586b"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , title = ChangedValue $ question11Edited.title
    , text = ChangedValue $ question11Edited.text
    , requiredPhaseUuid = ChangedValue $ question11Edited.requiredPhaseUuid
    , annotations = ChangedValue $ question11Edited.annotations
    , tagUuids = ChangedValue $ question11Edited.tagUuids
    , expertUuids = ChangedValue $ question11Edited.expertUuids
    , referenceUuids = ChangedValue $ question11Edited.referenceUuids
    , choiceUuids = ChangedValue $ question11Edited.choiceUuids
    , createdAt = dt' 2018 1 21
    }

d_km1_ch1_q1 :: DeleteQuestionEvent
d_km1_ch1_q1 =
  DeleteQuestionEvent
    { uuid = u' "aed9cf13-c81a-481f-bd8a-2689c4a74369"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , createdAt = dt' 2018 1 21
    }

d_km1_ch1_q1_2 :: DeleteQuestionEvent
d_km1_ch1_q1_2 =
  DeleteQuestionEvent
    { uuid = u' "aed9cf13-c81a-481f-bd8a-2689c4a74369"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , createdAt = dt' 2018 1 21
    }

d_km1_ch1_q2 :: DeleteQuestionEvent
d_km1_ch1_q2 =
  DeleteQuestionEvent
    { uuid = u' "52a7a6ae-be37-4075-ac5c-a20858707a75"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , createdAt = dt' 2018 1 21
    }

d_km1_ch1_q3 :: DeleteQuestionEvent
d_km1_ch1_q3 =
  DeleteQuestionEvent
    { uuid = u' "e46d208f-eb7d-48bc-8187-13a72b17ddb2"
    , parentUuid = chapter1.uuid
    , entityUuid = question3.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_aNo1 :: AddAnswerEvent
a_km1_ch1_q2_aNo1 =
  AddAnswerEvent
    { uuid = u' "afb36736-503a-43ca-a56b-8c144f89809e"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerNo.uuid
    , aLabel = q2_answerNo.aLabel
    , advice = q2_answerNo.advice
    , annotations = q2_answerNo.annotations
    , metricMeasures = q2_answerNo.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_aYes1 :: AddAnswerEvent
a_km1_ch1_q2_aYes1 =
  AddAnswerEvent
    { uuid = u' "e7ee93e4-18e7-4748-b0a5-781c77b8c937"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , aLabel = q2_answerYes.aLabel
    , advice = q2_answerYes.advice
    , annotations = q2_answerYes.annotations
    , metricMeasures = q2_answerYes.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_aMaybe :: AddAnswerEvent
a_km1_ch1_q2_aMaybe =
  AddAnswerEvent
    { uuid = u' "8ba60993-96ac-496b-9b8c-9580bf992cab"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerMaybe.uuid
    , aLabel = q2_answerMaybe.aLabel
    , advice = q2_answerMaybe.advice
    , annotations = q2_answerMaybe.annotations
    , metricMeasures = q2_answerMaybe.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_aYes1_fuq1_aNo :: AddAnswerEvent
a_km1_ch1_q2_aYes1_fuq1_aNo =
  AddAnswerEvent
    { uuid = u' "e62168e2-afe5-4e58-8ee7-555594aec23e"
    , parentUuid = q2_aYes_fuQuestion1.uuid
    , entityUuid = q2_aYes_fuq1_answerNo.uuid
    , aLabel = q2_aYes_fuq1_answerNo.aLabel
    , advice = q2_aYes_fuq1_answerNo.advice
    , annotations = q2_aYes_fuq1_answerNo.annotations
    , metricMeasures = q2_aYes_fuq1_answerNo.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_aYesFu1 :: AddAnswerEvent
a_km1_ch1_q2_aYesFu1 =
  AddAnswerEvent
    { uuid = u' "bc530681-b45b-4d36-b179-a9cb62a92838"
    , parentUuid = q2_aYes_fuQuestion1.uuid
    , entityUuid = q2_aYes_fuq1_answerYes.uuid
    , aLabel = q2_aYes_fuq1_answerYes.aLabel
    , advice = q2_aYes_fuq1_answerYes.advice
    , annotations = q2_aYes_fuq1_answerYes.annotations
    , metricMeasures = q2_aYes_fuq1_answerYes.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_aNoFu2 :: AddAnswerEvent
a_km1_ch1_q2_aNoFu2 =
  AddAnswerEvent
    { uuid = u' "abf67af9-23e0-43fa-a54a-746570882624"
    , parentUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuq2_answerNo.uuid
    , aLabel = q2_aYes_fuq1_aYes_fuq2_answerNo.aLabel
    , advice = q2_aYes_fuq1_aYes_fuq2_answerNo.advice
    , annotations = q2_aYes_fuq1_aYes_fuq2_answerNo.annotations
    , metricMeasures = q2_aYes_fuq1_aYes_fuq2_answerNo.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_aYesFu2 :: AddAnswerEvent
a_km1_ch1_q2_aYesFu2 =
  AddAnswerEvent
    { uuid = u' "542c0d28-9ae3-4bbe-8030-92a78b462276"
    , parentUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuq2_answerYes.uuid
    , aLabel = q2_aYes_fuq1_aYes_fuq2_answerYes.aLabel
    , advice = q2_aYes_fuq1_aYes_fuq2_answerYes.advice
    , annotations = q2_aYes_fuq1_aYes_fuq2_answerYes.annotations
    , metricMeasures = q2_aYes_fuq1_aYes_fuq2_answerYes.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q3_aNo2 :: AddAnswerEvent
a_km1_ch2_q3_aNo2 =
  AddAnswerEvent
    { uuid = u' "1bb10e82-33b5-4c98-b1d1-ab5413b5df66"
    , parentUuid = question3.uuid
    , entityUuid = q3_answerNo.uuid
    , aLabel = q3_answerNo.aLabel
    , advice = q3_answerNo.advice
    , annotations = q3_answerNo.annotations
    , metricMeasures = q3_answerNo.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q3_aYes2 :: AddAnswerEvent
a_km1_ch2_q3_aYes2 =
  AddAnswerEvent
    { uuid = u' "885ea1b9-0041-4240-911c-f35a9a6e4cbd"
    , parentUuid = question3.uuid
    , entityUuid = q3_answerYes.uuid
    , aLabel = q3_answerYes.aLabel
    , advice = q3_answerYes.advice
    , annotations = q3_answerYes.annotations
    , metricMeasures = q3_answerYes.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q4_it_q6_aNo :: AddAnswerEvent
a_km1_ch2_q4_it_q6_aNo =
  AddAnswerEvent
    { uuid = u' "c0a67ce5-21b3-47c7-8624-c2da26fb494f"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = q4_it1_q6_answerNo.uuid
    , aLabel = q4_it1_q6_answerNo.aLabel
    , advice = q4_it1_q6_answerNo.advice
    , annotations = q4_it1_q6_answerNo.annotations
    , metricMeasures = q4_it1_q6_answerNo.metricMeasures
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q4_it_q6_aYes :: AddAnswerEvent
a_km1_ch2_q4_it_q6_aYes =
  AddAnswerEvent
    { uuid = u' "c5c42f99-613b-4b6c-ae5e-af784f51c483"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = q4_it1_q6_answerYes.uuid
    , aLabel = q4_it1_q6_answerYes.aLabel
    , advice = q4_it1_q6_answerYes.advice
    , annotations = q4_it1_q6_answerYes.annotations
    , metricMeasures = q4_it1_q6_answerYes.metricMeasures
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_aYes1 :: EditAnswerEvent
e_km1_ch1_q2_aYes1 =
  EditAnswerEvent
    { uuid = u' "8c6632f6-0335-4912-924a-693a87cbe270"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , aLabel = ChangedValue $ q2_answerYesEdited.aLabel
    , advice = ChangedValue $ q2_answerYesEdited.advice
    , annotations = ChangedValue $ q2_answerYesEdited.annotations
    , followUpUuids = ChangedValue $ q2_answerYesEdited.followUpUuids
    , metricMeasures = ChangedValue $ q2_answerYesEdited.metricMeasures
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_aYes1_2 :: EditAnswerEvent
e_km1_ch1_q2_aYes1_2 =
  EditAnswerEvent
    { uuid = u' "8c6632f6-0335-4912-924a-693a87cbe270"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , aLabel = ChangedValue $ q2_answerYesEdited.aLabel
    , advice = ChangedValue $ q2_answerYesEdited.advice
    , annotations = ChangedValue $ q2_answerYesEdited.annotations
    , followUpUuids = ChangedValue $ q2_answerYes.followUpUuids
    , metricMeasures = ChangedValue $ q2_answerYes.metricMeasures
    , createdAt = dt' 2018 1 21
    }

d_km1_ch1_q2_aYes1 :: DeleteAnswerEvent
d_km1_ch1_q2_aYes1 =
  DeleteAnswerEvent
    { uuid = u' "1968692f-959a-4d47-b85f-d684eedb3e7f"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , createdAt = dt' 2018 1 21
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
    { uuid = u' "5619d036-0130-47fa-9553-b73094eecd7e"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question5.uuid
    , title = q4_it1_question5.title
    , text = q4_it1_question5.text
    , requiredPhaseUuid = q4_it1_question5.requiredPhaseUuid
    , annotations = q4_it1_question5.annotations
    , tagUuids = q4_it1_question5.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q4_it1_q6' :: AddQuestionEvent
a_km1_ch2_q4_it1_q6' = AddOptionsQuestionEvent' a_km1_ch2_q4_it1_q6

a_km1_ch2_q4_it1_q6 :: AddOptionsQuestionEvent
a_km1_ch2_q4_it1_q6 =
  AddOptionsQuestionEvent
    { uuid = u' "5ac56741-b93a-42f5-9beb-f22100e4342d"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question6.uuid
    , title = q4_it1_question6.title
    , text = q4_it1_question6.text
    , requiredPhaseUuid = q4_it1_question6.requiredPhaseUuid
    , annotations = q4_it1_question6.annotations
    , tagUuids = q4_it1_question6.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q4_it1_q6_fuq4_q1' :: AddQuestionEvent
a_km1_ch2_q4_it1_q6_fuq4_q1' = AddOptionsQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q1

a_km1_ch2_q4_it1_q6_fuq4_q1 :: AddOptionsQuestionEvent
a_km1_ch2_q4_it1_q6_fuq4_q1 =
  AddOptionsQuestionEvent
    { uuid = u' "55f46913-a953-4318-b72f-673e9f65fb2a"
    , parentUuid = q4_it1_q6_aYes_followUpQuestion4.uuid
    , entityUuid = q4_it1_q6_aYes_fuq4_it_question1.uuid
    , title = q4_it1_q6_aYes_fuq4_it_question1.title
    , text = q4_it1_q6_aYes_fuq4_it_question1.text
    , requiredPhaseUuid = q4_it1_q6_aYes_fuq4_it_question1.requiredPhaseUuid
    , annotations = q4_it1_q6_aYes_fuq4_it_question1.annotations
    , tagUuids = q4_it1_q6_aYes_fuq4_it_question1.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q4_it1_q6_fuq4_q2' :: AddQuestionEvent
a_km1_ch2_q4_it1_q6_fuq4_q2' = AddOptionsQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q2

a_km1_ch2_q4_it1_q6_fuq4_q2 :: AddOptionsQuestionEvent
a_km1_ch2_q4_it1_q6_fuq4_q2 =
  AddOptionsQuestionEvent
    { uuid = u' "6b9a7c1c-a23e-458a-a1bb-d7500c0ed96e"
    , parentUuid = q4_it1_q6_aYes_followUpQuestion4.uuid
    , entityUuid = q4_it1_q6_aYes_fuq4_it_question2.uuid
    , title = q4_it1_q6_aYes_fuq4_it_question2.title
    , text = q4_it1_q6_aYes_fuq4_it_question2.text
    , requiredPhaseUuid = q4_it1_q6_aYes_fuq4_it_question2.requiredPhaseUuid
    , annotations = q4_it1_q6_aYes_fuq4_it_question2.annotations
    , tagUuids = q4_it1_q6_aYes_fuq4_it_question2.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q4_it1_q7' :: AddQuestionEvent
a_km1_ch2_q4_it1_q7' = AddValueQuestionEvent' a_km1_ch2_q4_it1_q7

a_km1_ch2_q4_it1_q7 :: AddValueQuestionEvent
a_km1_ch2_q4_it1_q7 =
  AddValueQuestionEvent
    { uuid = u' "cf839365-91d0-427a-bb99-89de1a125929"
    , parentUuid = q4_it1_question5.uuid
    , entityUuid = q4_it1_q5_it2_question7.uuid
    , title = q4_it1_q5_it2_question7.title
    , text = q4_it1_q5_it2_question7.text
    , requiredPhaseUuid = q4_it1_q5_it2_question7.requiredPhaseUuid
    , annotations = q4_it1_q5_it2_question7.annotations
    , tagUuids = q4_it1_q5_it2_question7.tagUuids
    , valueType = q4_it1_q5_it2_question7.valueType
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q4_it1_q8' :: AddQuestionEvent
a_km1_ch2_q4_it1_q8' = AddValueQuestionEvent' a_km1_ch2_q4_it1_q8

a_km1_ch2_q4_it1_q8 :: AddValueQuestionEvent
a_km1_ch2_q4_it1_q8 =
  AddValueQuestionEvent
    { uuid = u' "3536a56f-d19c-4aff-ada1-ef7b3a60389d"
    , parentUuid = q4_it1_question5.uuid
    , entityUuid = q4_it1_q5_it2_question8.uuid
    , title = q4_it1_q5_it2_question8.title
    , text = q4_it1_q5_it2_question8.text
    , requiredPhaseUuid = q4_it1_q5_it2_question8.requiredPhaseUuid
    , annotations = q4_it1_q5_it2_question8.annotations
    , tagUuids = q4_it1_q5_it2_question8.tagUuids
    , valueType = q4_it1_q5_it2_question8.valueType
    , createdAt = dt' 2018 1 21
    }

e_km1_ch2_q4_it1_q5' :: EditQuestionEvent
e_km1_ch2_q4_it1_q5' = EditListQuestionEvent' e_km1_ch2_q4_it1_q5

e_km1_ch2_q4_it1_q5 :: EditListQuestionEvent
e_km1_ch2_q4_it1_q5 =
  EditListQuestionEvent
    { uuid = u' "17f8e9d4-7299-4c88-aba1-0a7b133aa8f3"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question5Edited.uuid
    , title = ChangedValue $ q4_it1_question5Edited.title
    , text = ChangedValue $ q4_it1_question5Edited.text
    , requiredPhaseUuid = ChangedValue $ q4_it1_question5Edited.requiredPhaseUuid
    , annotations = ChangedValue $ q4_it1_question5Edited.annotations
    , tagUuids = ChangedValue $ q4_it1_question5Edited.tagUuids
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , itemTemplateQuestionUuids =
        ChangedValue [q4_it1_q5_it2_question8.uuid, q4_it1_q5_it2_question7.uuid]
    , createdAt = dt' 2018 1 21
    }

e_km1_ch2_q4_it1_q6' :: EditQuestionEvent
e_km1_ch2_q4_it1_q6' = EditOptionsQuestionEvent' e_km1_ch2_q4_it1_q6

e_km1_ch2_q4_it1_q6 :: EditOptionsQuestionEvent
e_km1_ch2_q4_it1_q6 =
  EditOptionsQuestionEvent
    { uuid = u' "f5c5ccfd-619b-4110-807a-39ede6d31cae"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question6Edited.uuid
    , title = ChangedValue $ q4_it1_question6Edited.title
    , text = ChangedValue $ q4_it1_question6Edited.text
    , requiredPhaseUuid = ChangedValue $ q4_it1_question6Edited.requiredPhaseUuid
    , annotations = ChangedValue $ q4_it1_question6Edited.annotations
    , tagUuids = ChangedValue $ q4_it1_question6Edited.tagUuids
    , expertUuids = ChangedValue $ q4_it1_question6Edited.expertUuids
    , referenceUuids = ChangedValue $ q4_it1_question6Edited.referenceUuids
    , answerUuids = ChangedValue $ q4_it1_question6Edited.answerUuids
    , createdAt = dt' 2018 1 21
    }

d_km1_ch2_q4_it1_q5 :: DeleteQuestionEvent
d_km1_ch2_q4_it1_q5 =
  DeleteQuestionEvent
    { uuid = u' "424d19cb-a79f-4da0-b7f6-33363c32b7fd"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question5.uuid
    , createdAt = dt' 2018 1 21
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
    { uuid = u' "3588358c-159e-41a9-9847-262611007b61"
    , parentUuid = q2_answerYes.uuid
    , entityUuid = q2_aYes_fuQuestion1.uuid
    , title = q2_aYes_fuQuestion1.title
    , text = q2_aYes_fuQuestion1.text
    , requiredPhaseUuid = q2_aYes_fuQuestion1.requiredPhaseUuid
    , annotations = q2_aYes_fuQuestion1.annotations
    , tagUuids = q2_aYes_fuQuestion1.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2' :: AddQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2' = AddOptionsQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 :: AddOptionsQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 =
  AddOptionsQuestionEvent
    { uuid = u' "8ced5634-a879-4da2-b7c9-158ca6a4e0e3"
    , parentUuid = q2_aYes_fuq1_answerYes.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , title = q2_aYes_fuq1_aYes_fuQuestion2.title
    , text = q2_aYes_fuq1_aYes_fuQuestion2.text
    , requiredPhaseUuid = q2_aYes_fuq1_aYes_fuQuestion2.requiredPhaseUuid
    , annotations = q2_aYes_fuq1_aYes_fuQuestion2.annotations
    , tagUuids = q2_aYes_fuq1_aYes_fuQuestion2.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3' :: AddQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3' =
  AddOptionsQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 :: AddOptionsQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 =
  AddOptionsQuestionEvent
    { uuid = u' "6e9b591f-e6f9-46dd-85e8-a90fe4acc51c"
    , parentUuid = q2_aYes_fuq1_aYes_fuq2_answerYes.uuid
    , entityUuid = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.uuid
    , title = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.title
    , text = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.text
    , requiredPhaseUuid = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.requiredPhaseUuid
    , annotations = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.annotations
    , tagUuids = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_ansYes6_fuq4' :: AddQuestionEvent
a_km1_ch2_ansYes6_fuq4' = AddListQuestionEvent' a_km1_ch2_ansYes6_fuq4

a_km1_ch2_ansYes6_fuq4 :: AddListQuestionEvent
a_km1_ch2_ansYes6_fuq4 =
  AddListQuestionEvent
    { uuid = u' "c626fd42-80b8-4fd2-a16b-d38eeb8262f1"
    , parentUuid = q4_it1_q6_answerYes.uuid
    , entityUuid = q4_it1_q6_aYes_followUpQuestion4.uuid
    , title = q4_it1_q6_aYes_followUpQuestion4.title
    , text = q4_it1_q6_aYes_followUpQuestion4.text
    , requiredPhaseUuid = q4_it1_q6_aYes_followUpQuestion4.requiredPhaseUuid
    , annotations = q4_it1_q6_aYes_followUpQuestion4.annotations
    , tagUuids = q4_it1_q6_aYes_followUpQuestion4.tagUuids
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_ansYes6_fuq5' :: AddQuestionEvent
a_km1_ch2_ansYes6_fuq5' = AddIntegrationQuestionEvent' a_km1_ch2_ansYes6_fuq5

a_km1_ch2_ansYes6_fuq5 :: AddIntegrationQuestionEvent
a_km1_ch2_ansYes6_fuq5 =
  AddIntegrationQuestionEvent
    { uuid = u' "11872ad2-0d3d-4ab6-b81c-17d234bab6ba"
    , parentUuid = q4_it1_q6_answerYes.uuid
    , entityUuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , title = q4_it1_q6_aYes_followUpQuestion5.title
    , text = q4_it1_q6_aYes_followUpQuestion5.text
    , requiredPhaseUuid = q4_it1_q6_aYes_followUpQuestion5.requiredPhaseUuid
    , annotations = q4_it1_q6_aYes_followUpQuestion5.annotations
    , tagUuids = q4_it1_q6_aYes_followUpQuestion5.tagUuids
    , integrationUuid = q4_it1_q6_aYes_followUpQuestion5.integrationUuid
    , props = q4_it1_q6_aYes_followUpQuestion5.props
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2' :: EditQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2' = EditOptionsQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: EditOptionsQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  EditOptionsQuestionEvent
    { uuid = u' "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , parentUuid = q2_aYes_fuq1_answerYes.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , title = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.title
    , text = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.text
    , requiredPhaseUuid =
        ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.requiredPhaseUuid
    , annotations = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.annotations
    , tagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.tagUuids
    , expertUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.expertUuids
    , referenceUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.referenceUuids
    , answerUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.answerUuids
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2' :: EditQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2' = EditOptionsQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 :: EditOptionsQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 =
  EditOptionsQuestionEvent
    { uuid = u' "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , parentUuid = q2_aYes_fuq1_answerYes.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , title = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.title
    , text = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.text
    , requiredPhaseUuid =
        ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.requiredPhaseUuid
    , annotations = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.annotations
    , tagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.tagUuids
    , expertUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2.expertUuids
    , referenceUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2.referenceUuids
    , answerUuids =
        ChangedValue [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid, q2_aYes_fuq1_aYes_fuq2_answerNo.uuid]
    , createdAt = dt' 2018 1 21
    }

e_km1_ch2_ansMaybe6_fuq4' :: EditQuestionEvent
e_km1_ch2_ansMaybe6_fuq4' = EditListQuestionEvent' e_km1_ch2_ansMaybe6_fuq4

e_km1_ch2_ansMaybe6_fuq4 :: EditListQuestionEvent
e_km1_ch2_ansMaybe6_fuq4 =
  EditListQuestionEvent
    { uuid = u' "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , parentUuid = q4_it1_q6_answerNo.uuid
    , entityUuid = q4_it1_q6_aYes_followUpQuestion4Edited.uuid
    , title = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.title
    , text = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.text
    , requiredPhaseUuid =
        ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.requiredPhaseUuid
    , annotations = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.annotations
    , tagUuids = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.tagUuids
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , itemTemplateQuestionUuids =
        ChangedValue [q4_it1_q6_aYes_fuq4_it_question2.uuid, q4_it1_q6_aYes_fuq4_it_question1.uuid]
    , createdAt = dt' 2018 1 21
    }

d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: DeleteQuestionEvent
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  DeleteQuestionEvent
    { uuid = u' "db69d694-cfb6-4461-8a13-81c01638f348"
    , parentUuid = q2_aYes_fuq1_answerYes.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch3_q11_cho1 :: AddChoiceEvent
a_km1_ch3_q11_cho1 =
  AddChoiceEvent
    { uuid = u' "0a58e6bf-a185-400f-945a-17a96fac6073"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice1.uuid
    , aLabel = q11_choice1.aLabel
    , annotations = q11_choice1.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch3_q11_cho2 :: AddChoiceEvent
a_km1_ch3_q11_cho2 =
  AddChoiceEvent
    { uuid = u' "da967bd5-4eb3-4329-ad79-63f49ad361c3"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice2.uuid
    , aLabel = q11_choice2.aLabel
    , annotations = q11_choice2.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch3_q11_cho3 :: AddChoiceEvent
a_km1_ch3_q11_cho3 =
  AddChoiceEvent
    { uuid = u' "1c8561ae-44fb-4e5e-96e7-2582563330de"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice3.uuid
    , aLabel = q11_choice3.aLabel
    , annotations = q11_choice3.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_ch3_q11_cho1 :: EditChoiceEvent
e_km1_ch3_q11_cho1 =
  EditChoiceEvent
    { uuid = u' "bda5b518-f7f0-4ea3-b609-9117f5931c54"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice1Edited.uuid
    , aLabel = ChangedValue $ q11_choice1Edited.aLabel
    , annotations = ChangedValue $ q11_choice1Edited.annotations
    , createdAt = dt' 2018 1 21
    }

d_km1_ch3_q11_cho1 :: DeleteChoiceEvent
d_km1_ch3_q11_cho1 =
  DeleteChoiceEvent
    { uuid = u' "9f877d39-103c-494a-b863-19050029242c"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice1.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_eAlbert :: AddExpertEvent
a_km1_ch1_q2_eAlbert =
  AddExpertEvent
    { uuid = u' "ec76054f-d059-4a5f-81c9-1817004a913c"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eAlbert.uuid
    , name = km1_ch1_q2_eAlbert.name
    , email = km1_ch1_q2_eAlbert.email
    , annotations = km1_ch1_q2_eAlbert.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q6_eAlbert :: AddExpertEvent
a_km1_ch2_q6_eAlbert =
  AddExpertEvent
    { uuid = u' "eb6bb073-ecba-4cd0-91a3-ff31d374601f"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = km1_ch2_q6_eAlbert.uuid
    , name = km1_ch2_q6_eAlbert.name
    , email = km1_ch2_q6_eAlbert.email
    , annotations = km1_ch2_q6_eAlbert.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_eNikola :: AddExpertEvent
a_km1_ch1_q2_eNikola =
  AddExpertEvent
    { uuid = u' "40bb45bd-4195-4430-ac8f-16ac5a61ece0"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eNikola.uuid
    , name = km1_ch1_q2_eNikola.name
    , email = km1_ch1_q2_eNikola.email
    , annotations = km1_ch1_q2_eNikola.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q6_eNikola :: AddExpertEvent
a_km1_ch2_q6_eNikola =
  AddExpertEvent
    { uuid = u' "53653d05-6d5a-4b76-bbc6-15ca8314ad69"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = km1_ch2_q6_eNikola.uuid
    , name = km1_ch2_q6_eNikola.name
    , email = km1_ch2_q6_eNikola.email
    , annotations = km1_ch2_q6_eNikola.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_eIsaac :: AddExpertEvent
a_km1_ch1_q2_eIsaac =
  AddExpertEvent
    { uuid = u' "2d5eedae-1782-44ac-9d4e-3db769161448"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eIsaac.uuid
    , name = km1_ch1_q2_eIsaac.name
    , email = km1_ch1_q2_eIsaac.email
    , annotations = km1_ch1_q2_eIsaac.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_eAlbert :: EditExpertEvent
e_km1_ch1_q2_eAlbert =
  EditExpertEvent
    { uuid = u' "01686131-2423-4d97-a949-4fea2c9ce3b7"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eAlbertEdited.uuid
    , name = ChangedValue $ km1_ch1_q2_eAlbertEdited.name
    , email = ChangedValue $ km1_ch1_q2_eAlbertEdited.email
    , annotations = ChangedValue $ km1_ch1_q2_eAlbertEdited.annotations
    , createdAt = dt' 2018 1 21
    }

d_km1_ch1_q2_eNikola :: DeleteExpertEvent
d_km1_ch1_q2_eNikola =
  DeleteExpertEvent
    { uuid = u' "f20bc988-6d44-4051-990d-d16b24f369ac"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eNikola.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_rCh1' :: AddReferenceEvent
a_km1_ch1_q2_rCh1' = AddResourcePageReferenceEvent' a_km1_ch1_q2_rCh1

a_km1_ch1_q2_rCh1 :: AddResourcePageReferenceEvent
a_km1_ch1_q2_rCh1 =
  AddResourcePageReferenceEvent
    { uuid = u' "1177d72f-b7d8-466d-ad33-d5f82d0f192a"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r1.uuid
    , shortUuid = km1_ch1_q2_r1.shortUuid
    , annotations = km1_ch1_q2_r1.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q6_rCh1' :: AddReferenceEvent
a_km1_ch2_q6_rCh1' = AddResourcePageReferenceEvent' a_km1_ch2_q6_rCh1

a_km1_ch2_q6_rCh1 :: AddResourcePageReferenceEvent
a_km1_ch2_q6_rCh1 =
  AddResourcePageReferenceEvent
    { uuid = u' "a3f6ee9a-803f-4911-9566-734a6358913a"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = km1_ch2_q6_r1.uuid
    , shortUuid = km1_ch2_q6_r1.shortUuid
    , annotations = km1_ch2_q6_r1.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_rCh2' :: AddReferenceEvent
a_km1_ch1_q2_rCh2' = AddURLReferenceEvent' a_km1_ch1_q2_rCh2

a_km1_ch1_q2_rCh2 :: AddURLReferenceEvent
a_km1_ch1_q2_rCh2 =
  AddURLReferenceEvent
    { uuid = u' "4814f50f-8838-4b53-8b18-c0f8c568220e"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r2.uuid
    , url = km1_ch1_q2_r2.url
    , aLabel = km1_ch1_q2_r2.aLabel
    , annotations = km1_ch1_q2_r2.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch2_q6_rCh2' :: AddReferenceEvent
a_km1_ch2_q6_rCh2' = AddURLReferenceEvent' a_km1_ch2_q6_rCh2

a_km1_ch2_q6_rCh2 :: AddURLReferenceEvent
a_km1_ch2_q6_rCh2 =
  AddURLReferenceEvent
    { uuid = u' "a4ae3400-dd3c-41ab-b796-4bf9d0bdafe7"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = km1_ch2_q6_r2.uuid
    , url = km1_ch2_q6_r2.url
    , aLabel = km1_ch2_q6_r2.aLabel
    , annotations = km1_ch2_q6_r2.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ch1_q2_rCh3' :: AddReferenceEvent
a_km1_ch1_q2_rCh3' = AddCrossReferenceEvent' a_km1_ch1_q2_rCh3

a_km1_ch1_q2_rCh3 :: AddCrossReferenceEvent
a_km1_ch1_q2_rCh3 =
  AddCrossReferenceEvent
    { uuid = u' "45d8ec86-34bc-4e8f-b42a-48a567a77d8b"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r3.uuid
    , targetUuid = km1_ch1_q2_r3.targetUuid
    , description = km1_ch1_q2_r3.description
    , annotations = km1_ch1_q2_r3.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_rCh1' :: EditReferenceEvent
e_km1_ch1_q2_rCh1' = EditResourcePageReferenceEvent' e_km1_ch1_q2_rCh1

e_km1_ch1_q2_rCh1 :: EditResourcePageReferenceEvent
e_km1_ch1_q2_rCh1 =
  EditResourcePageReferenceEvent
    { uuid = u' "08cd9afc-d416-48ab-8669-17e87ceb15dc"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r1Edited.uuid
    , shortUuid = ChangedValue $ km1_ch1_q2_r1Edited.shortUuid
    , annotations = ChangedValue $ km1_ch1_q2_r1Edited.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_rCh1_type' :: EditReferenceEvent
e_km1_ch1_q2_rCh1_type' = EditURLReferenceEvent' e_km1_ch1_q2_rCh1_type

e_km1_ch1_q2_rCh1_type :: EditURLReferenceEvent
e_km1_ch1_q2_rCh1_type =
  EditURLReferenceEvent
    { uuid = u' "4e1058cf-9044-42a0-901c-816bd6847b17"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r1WithNewType.uuid
    , url = ChangedValue $ km1_ch1_q2_r1WithNewType.url
    , aLabel = ChangedValue $ km1_ch1_q2_r1WithNewType.aLabel
    , annotations = ChangedValue $ km1_ch1_q2_r1WithNewType.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_rCh2' :: EditReferenceEvent
e_km1_ch1_q2_rCh2' = EditURLReferenceEvent' e_km1_ch1_q2_rCh2

e_km1_ch1_q2_rCh2 :: EditURLReferenceEvent
e_km1_ch1_q2_rCh2 =
  EditURLReferenceEvent
    { uuid = u' "f96588ae-1657-406e-9810-1d00f5e24a96"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r2Edited.uuid
    , url = ChangedValue $ km1_ch1_q2_r2Edited.url
    , aLabel = ChangedValue $ km1_ch1_q2_r2Edited.aLabel
    , annotations = ChangedValue $ km1_ch1_q2_r2Edited.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_rCh2_type' :: EditReferenceEvent
e_km1_ch1_q2_rCh2_type' = EditCrossReferenceEvent' e_km1_ch1_q2_rCh2_type

e_km1_ch1_q2_rCh2_type :: EditCrossReferenceEvent
e_km1_ch1_q2_rCh2_type =
  EditCrossReferenceEvent
    { uuid = u' "e0a19e9d-fb36-47b3-bc23-f752f7403937"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r2WithNewType.uuid
    , targetUuid = ChangedValue $ km1_ch1_q2_r2WithNewType.targetUuid
    , description = ChangedValue $ km1_ch1_q2_r2WithNewType.description
    , annotations = ChangedValue $ km1_ch1_q2_r2WithNewType.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_rCh3' :: EditReferenceEvent
e_km1_ch1_q2_rCh3' = EditCrossReferenceEvent' e_km1_ch1_q2_rCh3

e_km1_ch1_q2_rCh3 :: EditCrossReferenceEvent
e_km1_ch1_q2_rCh3 =
  EditCrossReferenceEvent
    { uuid = u' "d3a7b6a6-9e87-4308-a103-88245537c26e"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r3Edited.uuid
    , targetUuid = ChangedValue $ km1_ch1_q2_r3Edited.targetUuid
    , description = ChangedValue $ km1_ch1_q2_r3Edited.description
    , annotations = ChangedValue $ km1_ch1_q2_r3Edited.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_ch1_q2_rCh3_type' :: EditReferenceEvent
e_km1_ch1_q2_rCh3_type' = EditResourcePageReferenceEvent' e_km1_ch1_q2_rCh3_type

e_km1_ch1_q2_rCh3_type :: EditResourcePageReferenceEvent
e_km1_ch1_q2_rCh3_type =
  EditResourcePageReferenceEvent
    { uuid = u' "f8528e3b-4904-4ad8-87b8-809d7e40c087"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r3WithNewType.uuid
    , shortUuid = ChangedValue $ km1_ch1_q2_r3WithNewType.shortUuid
    , annotations = ChangedValue $ km1_ch1_q2_r3WithNewType.annotations
    , createdAt = dt' 2018 1 21
    }

d_km1_ch1_q2_rCh2 :: DeleteReferenceEvent
d_km1_ch1_q2_rCh2 =
  DeleteReferenceEvent
    { uuid = u' "3cc15f31-4801-404f-ba48-6b91f77d1abe"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r2.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_tds :: AddTagEvent
a_km1_tds =
  AddTagEvent
    { uuid = u' "dedc4a9d-00d9-41b6-8494-a10a238be03b"
    , parentUuid = km1.uuid
    , entityUuid = tagDataScience.uuid
    , name = tagDataScience.name
    , description = tagDataScience.description
    , color = tagDataScience.color
    , annotations = tagDataScience.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_tbi :: AddTagEvent
a_km1_tbi =
  AddTagEvent
    { uuid = u' "b6b0e53c-5702-403c-950c-e04960e09e73"
    , parentUuid = km1.uuid
    , entityUuid = tagBioInformatic.uuid
    , name = tagBioInformatic.name
    , description = tagBioInformatic.description
    , color = tagBioInformatic.color
    , annotations = tagBioInformatic.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_tds :: EditTagEvent
e_km1_tds =
  EditTagEvent
    { uuid = u' "f68f764b-48d1-4b30-8d53-48cfa2752801"
    , parentUuid = km1.uuid
    , entityUuid = tagDataScienceEdited.uuid
    , name = ChangedValue $ tagDataScienceEdited.name
    , description = ChangedValue $ tagDataScienceEdited.description
    , color = ChangedValue $ tagDataScienceEdited.color
    , annotations = ChangedValue $ tagDataScienceEdited.annotations
    , createdAt = dt' 2018 1 21
    }

d_km1_tds :: DeleteTagEvent
d_km1_tds =
  DeleteTagEvent
    { uuid = u' "969d00c2-062d-4763-a372-536d486c532f"
    , parentUuid = km1.uuid
    , entityUuid = tagDataScience.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_iop' :: AddIntegrationEvent
a_km1_iop' = AddApiIntegrationEvent' a_km1_iop

a_km1_iop :: AddApiIntegrationEvent
a_km1_iop =
  AddApiIntegrationEvent
    { uuid = u' "3f94cb01-6f92-4eb6-975b-385c02b831bc"
    , parentUuid = km1.uuid
    , entityUuid = ontologyPortal.uuid
    , iId = ontologyPortal.iId
    , name = ontologyPortal.name
    , props = ontologyPortal.props
    , logo = ontologyPortal.logo
    , requestMethod = ontologyPortal.requestMethod
    , requestUrl = ontologyPortal.requestUrl
    , requestHeaders = ontologyPortal.requestHeaders
    , requestBody = ontologyPortal.requestBody
    , requestEmptySearch = ontologyPortal.requestEmptySearch
    , responseListField = ontologyPortal.responseListField
    , responseItemId = ontologyPortal.responseItemId
    , responseItemTemplate = ontologyPortal.responseItemTemplate
    , itemUrl = ontologyPortal.itemUrl
    , annotations = ontologyPortal.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_ibp' :: AddIntegrationEvent
a_km1_ibp' = AddApiIntegrationEvent' a_km1_ibp

a_km1_ibp :: AddApiIntegrationEvent
a_km1_ibp =
  AddApiIntegrationEvent
    { uuid = u' "5c47b31c-84d0-4792-99ce-09154642105d"
    , parentUuid = km1.uuid
    , entityUuid = bioPortal.uuid
    , iId = bioPortal.iId
    , name = bioPortal.name
    , props = bioPortal.props
    , logo = bioPortal.logo
    , requestMethod = bioPortal.requestMethod
    , requestUrl = bioPortal.requestUrl
    , requestHeaders = bioPortal.requestHeaders
    , requestBody = bioPortal.requestBody
    , requestEmptySearch = bioPortal.requestEmptySearch
    , responseListField = bioPortal.responseListField
    , responseItemId = bioPortal.responseItemId
    , responseItemTemplate = bioPortal.responseItemTemplate
    , itemUrl = bioPortal.itemUrl
    , annotations = bioPortal.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_iwp' :: AddIntegrationEvent
a_km1_iwp' = AddWidgetIntegrationEvent' a_km1_iwp

a_km1_iwp :: AddWidgetIntegrationEvent
a_km1_iwp =
  AddWidgetIntegrationEvent
    { uuid = u' "cd3275d5-1c51-4609-bf6e-3bf1b2070dd5"
    , parentUuid = km1.uuid
    , entityUuid = widgetPortal.uuid
    , iId = widgetPortal.iId
    , name = widgetPortal.name
    , props = widgetPortal.props
    , logo = widgetPortal.logo
    , widgetUrl = widgetPortal.widgetUrl
    , itemUrl = widgetPortal.itemUrl
    , annotations = widgetPortal.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_iop' :: EditIntegrationEvent
e_km1_iop' = EditApiIntegrationEvent' e_km1_iop

e_km1_iop :: EditApiIntegrationEvent
e_km1_iop =
  EditApiIntegrationEvent
    { uuid = u' "3456a254-c5bc-4c0e-8ff9-f5e080765a71"
    , parentUuid = km1.uuid
    , entityUuid = ontologyPortalEdited.uuid
    , iId = ChangedValue $ ontologyPortalEdited.iId
    , name = ChangedValue $ ontologyPortalEdited.name
    , props = ChangedValue $ ontologyPortalEdited.props
    , logo = ChangedValue $ ontologyPortalEdited.logo
    , requestMethod = ChangedValue $ ontologyPortalEdited.requestMethod
    , requestUrl = ChangedValue $ ontologyPortalEdited.requestUrl
    , requestHeaders = ChangedValue $ ontologyPortalEdited.requestHeaders
    , requestBody = ChangedValue $ ontologyPortalEdited.requestBody
    , requestEmptySearch = ChangedValue $ ontologyPortalEdited.requestEmptySearch
    , responseListField = ChangedValue $ ontologyPortalEdited.responseListField
    , responseItemId = ChangedValue $ ontologyPortalEdited.responseItemId
    , responseItemTemplate = ChangedValue $ ontologyPortalEdited.responseItemTemplate
    , itemUrl = ChangedValue $ ontologyPortalEdited.itemUrl
    , annotations = ChangedValue $ ontologyPortalEdited.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_iwp' :: EditIntegrationEvent
e_km1_iwp' = EditWidgetIntegrationEvent' e_km1_iwp

e_km1_iwp :: EditWidgetIntegrationEvent
e_km1_iwp =
  EditWidgetIntegrationEvent
    { uuid = u' "2c62e0d2-7e5f-4acb-9b8e-826202fc4fa9"
    , parentUuid = km1.uuid
    , entityUuid = widgetPortalEdited.uuid
    , iId = ChangedValue $ widgetPortalEdited.iId
    , name = ChangedValue $ widgetPortalEdited.name
    , props = ChangedValue $ widgetPortalEdited.props
    , logo = ChangedValue $ widgetPortalEdited.logo
    , widgetUrl = ChangedValue $ widgetPortalEdited.widgetUrl
    , itemUrl = ChangedValue $ widgetPortalEdited.itemUrl
    , annotations = ChangedValue $ widgetPortalEdited.annotations
    , createdAt = dt' 2018 1 21
    }

d_km1_iop :: DeleteIntegrationEvent
d_km1_iop =
  DeleteIntegrationEvent
    { uuid = u' "d211d46f-5358-497a-92a0-e0bde08ce3d3"
    , parentUuid = km1.uuid
    , entityUuid = ontologyPortal.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_mtrF :: AddMetricEvent
a_km1_mtrF =
  AddMetricEvent
    { uuid = u' "d22017a1-89ea-4aba-b2df-92ea2cf4eac5"
    , parentUuid = km1.uuid
    , entityUuid = metricF.uuid
    , title = metricF.title
    , abbreviation = metricF.abbreviation
    , description = metricF.description
    , annotations = metricF.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_mtrA :: AddMetricEvent
a_km1_mtrA =
  AddMetricEvent
    { uuid = u' "d7d4052e-5413-48ec-8e0e-0b43e027369e"
    , parentUuid = km1.uuid
    , entityUuid = metricA.uuid
    , title = metricA.title
    , abbreviation = metricA.abbreviation
    , description = metricA.description
    , annotations = metricA.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_mtrI :: AddMetricEvent
a_km1_mtrI =
  AddMetricEvent
    { uuid = u' "6b6e0cb2-5f1d-42ed-9576-c454664a7884"
    , parentUuid = km1.uuid
    , entityUuid = metricI.uuid
    , title = metricI.title
    , abbreviation = metricI.abbreviation
    , description = metricI.description
    , annotations = metricI.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_mtrR :: AddMetricEvent
a_km1_mtrR =
  AddMetricEvent
    { uuid = u' "6d62e9fe-0a67-4f63-8ff8-4553f1154018"
    , parentUuid = km1.uuid
    , entityUuid = metricR.uuid
    , title = metricR.title
    , abbreviation = metricR.abbreviation
    , description = metricR.description
    , annotations = metricR.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_mtrG :: AddMetricEvent
a_km1_mtrG =
  AddMetricEvent
    { uuid = u' "84fa1ecf-a445-4a54-a1d5-34062ddc7735"
    , parentUuid = km1.uuid
    , entityUuid = metricG.uuid
    , title = metricG.title
    , abbreviation = metricG.abbreviation
    , description = metricG.description
    , annotations = metricG.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_mtrO :: AddMetricEvent
a_km1_mtrO =
  AddMetricEvent
    { uuid = u' "c7b2f5a9-1b18-44ea-9296-259335e410f5"
    , parentUuid = km1.uuid
    , entityUuid = metricO.uuid
    , title = metricO.title
    , abbreviation = metricO.abbreviation
    , description = metricO.description
    , annotations = metricO.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_mtrF :: EditMetricEvent
e_km1_mtrF =
  EditMetricEvent
    { uuid = u' "da2350c5-b881-4e46-a8b1-94d476d1fc74"
    , parentUuid = km1.uuid
    , entityUuid = metricFEdited.uuid
    , title = ChangedValue $ metricFEdited.title
    , abbreviation = ChangedValue $ metricFEdited.abbreviation
    , description = ChangedValue $ metricFEdited.description
    , annotations = ChangedValue $ metricFEdited.annotations
    , createdAt = dt' 2018 1 21
    }

d_km1_mtrF :: DeleteMetricEvent
d_km1_mtrF =
  DeleteMetricEvent
    { uuid = u' "e1b1a8ed-f23d-49aa-80a9-2077055aac87"
    , parentUuid = km1.uuid
    , entityUuid = metricF.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_phs1 :: AddPhaseEvent
a_km1_phs1 =
  AddPhaseEvent
    { uuid = u' "e3ba08a4-1775-4a74-b062-625c18afa65f"
    , parentUuid = km1.uuid
    , entityUuid = phase1.uuid
    , title = phase1.title
    , description = phase1.description
    , annotations = phase1.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_phs2 :: AddPhaseEvent
a_km1_phs2 =
  AddPhaseEvent
    { uuid = u' "4853d211-17fd-46fa-8327-d45a58a6eb12"
    , parentUuid = km1.uuid
    , entityUuid = phase2.uuid
    , title = phase2.title
    , description = phase2.description
    , annotations = phase2.annotations
    , createdAt = dt' 2018 1 21
    }

a_km1_phs3 :: AddPhaseEvent
a_km1_phs3 =
  AddPhaseEvent
    { uuid = u' "e1c813ec-1ee2-46be-bc85-4386aef91657"
    , parentUuid = km1.uuid
    , entityUuid = phase3.uuid
    , title = phase3.title
    , description = phase3.description
    , annotations = phase3.annotations
    , createdAt = dt' 2018 1 21
    }

e_km1_phs1 :: EditPhaseEvent
e_km1_phs1 =
  EditPhaseEvent
    { uuid = u' "d7e65e08-52bc-4096-a24c-1dc737e64266"
    , parentUuid = km1.uuid
    , entityUuid = phase1Edited.uuid
    , title = ChangedValue $ phase1Edited.title
    , description = ChangedValue $ phase1Edited.description
    , annotations = ChangedValue $ phase1Edited.annotations
    , createdAt = dt' 2018 1 21
    }

d_km1_phs1 :: DeletePhaseEvent
d_km1_phs1 =
  DeletePhaseEvent
    { uuid = u' "18ea7949-7e5a-4fae-8f5f-67b509ae397a"
    , parentUuid = km1.uuid
    , entityUuid = phase1.uuid
    , createdAt = dt' 2018 1 21
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
m_km1_ch1_q1__to_ch2 :: MoveQuestionEvent
m_km1_ch1_q1__to_ch2 =
  MoveQuestionEvent
    { uuid = u' "f13a1d1b-5cb6-458a-ad99-cafe3912aa1d"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , targetUuid = chapter2.uuid
    , createdAt = dt' 2018 1 21
    }

m_km1_ch1_q1__to_ch2_q3_aNo :: MoveQuestionEvent
m_km1_ch1_q1__to_ch2_q3_aNo =
  MoveQuestionEvent
    { uuid = u' "3bf501a1-cbc2-4b94-9b17-d23f0bad7fc9"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , targetUuid = q3_answerNo.uuid
    , createdAt = dt' 2018 1 21
    }

m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo :: MoveQuestionEvent
m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo =
  MoveQuestionEvent
    { uuid = u' "a2f35e98-dd67-45cf-a18e-a8a38382c7be"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question5.uuid
    , targetUuid = q4_it1_q6_answerNo.uuid
    , createdAt = dt' 2018 1 21
    }

m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4 :: MoveQuestionEvent
m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4 =
  MoveQuestionEvent
    { uuid = u' "a2f35e98-dd67-45cf-a18e-a8a38382c7be"
    , parentUuid = q4_it1_q6_aYes_followUpQuestion4.uuid
    , entityUuid = q4_it1_q6_aYes_fuq4_it_question1.uuid
    , targetUuid = question4.uuid
    , createdAt = dt' 2018 1 21
    }

m_km1_ch1_q2_aYes__to_ch2_q3 :: MoveAnswerEvent
m_km1_ch1_q2_aYes__to_ch2_q3 =
  MoveAnswerEvent
    { uuid = u' "b660447a-ddbd-482a-9610-68dfca6a25fd"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , targetUuid = question3.uuid
    , createdAt = dt' 2018 1 21
    }

m_km1_ch3_q11_cho1__to_ch3_q12 :: MoveChoiceEvent
m_km1_ch3_q11_cho1__to_ch3_q12 =
  MoveChoiceEvent
    { uuid = u' "0ffdff49-db85-4f28-b8a9-6b7a1569f5fd"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice1.uuid
    , targetUuid = question12.uuid
    , createdAt = dt' 2018 1 21
    }

m_km1_ch1_q2_eAlbert__to_ch2_q3 :: MoveExpertEvent
m_km1_ch1_q2_eAlbert__to_ch2_q3 =
  MoveExpertEvent
    { uuid = u' "35b18cb0-912f-4c76-9f80-b6bfc6479c7c"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eAlbert.uuid
    , targetUuid = question3.uuid
    , createdAt = dt' 2018 1 21
    }

m_km1_ch1_q2_r1__to_ch2_q3 :: MoveReferenceEvent
m_km1_ch1_q2_r1__to_ch2_q3 =
  MoveReferenceEvent
    { uuid = u' "1cc9ad2b-22bc-4806-902e-49b46ccc14d5"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r1.uuid
    , targetUuid = question3.uuid
    , createdAt = dt' 2018 1 21
    }
