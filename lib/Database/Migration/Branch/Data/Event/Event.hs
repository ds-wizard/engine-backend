module Database.Migration.Branch.Data.Event.Event where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions
import Database.Migration.Branch.Data.KnowledgeModel.Chapters
import Database.Migration.Branch.Data.KnowledgeModel.Experts
import Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
import Database.Migration.Branch.Data.KnowledgeModel.Questions
import Database.Migration.Branch.Data.KnowledgeModel.References
import LensesConfig
import Model.Common
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Event
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors

a_km1 :: AddKnowledgeModelEvent
a_km1 =
  AddKnowledgeModelEvent
  { _addKnowledgeModelEventUuid = fromJust $ U.fromString "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f"
  , _addKnowledgeModelEventKmUuid = km1WithoutChapters ^. uuid
  , _addKnowledgeModelEventName = km1WithoutChapters ^. name
  }

e_km1 :: EditKnowledgeModelEvent
e_km1 =
  EditKnowledgeModelEvent
  { _editKnowledgeModelEventUuid = fromJust $ U.fromString "8294a55d-642d-416c-879b-5a42a4430c24"
  , _editKnowledgeModelEventKmUuid = km1 ^. uuid
  , _editKnowledgeModelEventName = Just $ km1WithChangeProperties ^. name
  , _editKnowledgeModelEventChapterIds = Just $ getChapterIds km1WithChangeProperties
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1 :: AddChapterEvent
a_km1_ch1 =
  AddChapterEvent
  { _addChapterEventUuid = fromJust $ U.fromString "dedc4a9d-00d9-41b6-8494-a10a238be03b"
  , _addChapterEventKmUuid = km1 ^. uuid
  , _addChapterEventChapterUuid = chapter1WithoutQuestions ^. uuid
  , _addChapterEventTitle = chapter1WithoutQuestions ^. title
  , _addChapterEventText = chapter1WithoutQuestions ^. text
  }

a_km1_ch2 :: AddChapterEvent
a_km1_ch2 =
  AddChapterEvent
  { _addChapterEventUuid = fromJust $ U.fromString "6c4bba6e-864b-4871-98ca-49ac7a3e5eb5"
  , _addChapterEventKmUuid = km1 ^. uuid
  , _addChapterEventChapterUuid = chapter2WithoutQuestions ^. uuid
  , _addChapterEventTitle = chapter2WithoutQuestions ^. title
  , _addChapterEventText = chapter2WithoutQuestions ^. text
  }

a_km1_ch3 :: AddChapterEvent
a_km1_ch3 =
  AddChapterEvent
  { _addChapterEventUuid = fromJust $ U.fromString "6585a64d-c75b-47fc-a86e-e0c8e773528f"
  , _addChapterEventKmUuid = km1 ^. uuid
  , _addChapterEventChapterUuid = chapter3WithoutQuestions ^. uuid
  , _addChapterEventTitle = chapter3WithoutQuestions ^. title
  , _addChapterEventText = chapter3WithoutQuestions ^. text
  }

e_km1_ch1 :: EditChapterEvent
e_km1_ch1 =
  EditChapterEvent
  { _editChapterEventUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _editChapterEventKmUuid = km1 ^. uuid
  , _editChapterEventChapterUuid = chapter1 ^. uuid
  , _editChapterEventTitle = Just $ chapter1WithChangeProperties ^. title
  , _editChapterEventText = Just $ chapter1WithChangeProperties ^. text
  , _editChapterEventQuestionIds = Just $ getQuestionIds chapter1WithChangeProperties
  }

e_km1_ch1_2 :: EditChapterEvent
e_km1_ch1_2 =
  EditChapterEvent
  { _editChapterEventUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _editChapterEventKmUuid = km1 ^. uuid
  , _editChapterEventChapterUuid = chapter1 ^. uuid
  , _editChapterEventTitle = Just $ "TWICE: " ++ chapter1WithChangeProperties ^. title
  , _editChapterEventText = Just $ chapter1WithChangeProperties ^. text
  , _editChapterEventQuestionIds = Just $ getQuestionIds chapter1WithChangeProperties
  }

d_km1_ch1 :: DeleteChapterEvent
d_km1_ch1 =
  DeleteChapterEvent
  { _deleteChapterEventUuid = fromJust $ U.fromString "d07cc69b-abd3-43ec-bce1-fe59899dbda3"
  , _deleteChapterEventKmUuid = km1 ^. uuid
  , _deleteChapterEventChapterUuid = chapter1 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q1 :: AddQuestionEvent
a_km1_ch1_q1 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "71ae2ce9-553b-4ca2-a542-1bce04406c51"
  , _addQuestionEventKmUuid = km1 ^. uuid
  , _addQuestionEventChapterUuid = chapter1 ^. uuid
  , _addQuestionEventQuestionUuid = question1 ^. uuid
  , _addQuestionEventShortQuestionUuid = question1 ^. shortUuid
  , _addQuestionEventQType = question1 ^. qType
  , _addQuestionEventTitle = question1 ^. title
  , _addQuestionEventText = question1 ^. text
  }

a_km1_ch1_q2 :: AddQuestionEvent
a_km1_ch1_q2 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "ced9be29-24af-4443-8f5f-e709791a8fe3"
  , _addQuestionEventKmUuid = km1 ^. uuid
  , _addQuestionEventChapterUuid = chapter1 ^. uuid
  , _addQuestionEventQuestionUuid = question2 ^. uuid
  , _addQuestionEventShortQuestionUuid = question2 ^. shortUuid
  , _addQuestionEventQType = question2 ^. qType
  , _addQuestionEventTitle = question2 ^. title
  , _addQuestionEventText = question2 ^. text
  }

a_km1_ch1_q3 :: AddQuestionEvent
a_km1_ch1_q3 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "d559ac95-cc81-4502-a780-dbaee46f24bc"
  , _addQuestionEventKmUuid = km1 ^. uuid
  , _addQuestionEventChapterUuid = chapter1 ^. uuid
  , _addQuestionEventQuestionUuid = question3 ^. uuid
  , _addQuestionEventShortQuestionUuid = question3 ^. shortUuid
  , _addQuestionEventQType = question3 ^. qType
  , _addQuestionEventTitle = question3 ^. title
  , _addQuestionEventText = question3 ^. text
  }

a_km1_ch2_q3 :: AddQuestionEvent
a_km1_ch2_q3 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
  , _addQuestionEventKmUuid = km1 ^. uuid
  , _addQuestionEventChapterUuid = chapter2 ^. uuid
  , _addQuestionEventQuestionUuid = question3 ^. uuid
  , _addQuestionEventShortQuestionUuid = question3 ^. shortUuid
  , _addQuestionEventQType = question3 ^. qType
  , _addQuestionEventTitle = question3 ^. title
  , _addQuestionEventText = question3 ^. text
  }

e_km1_ch1_q1_title :: EditQuestionEvent
e_km1_ch1_q1_title =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "de86f82b-aaaf-482e-97c7-c7e93d834cd9"
  , _editQuestionEventKmUuid = km1 ^. uuid
  , _editQuestionEventChapterUuid = chapter1 ^. uuid
  , _editQuestionEventQuestionUuid = question1 ^. uuid
  , _editQuestionEventShortQuestionUuid = Nothing
  , _editQuestionEventQType = Nothing
  , _editQuestionEventTitle = Just $ "EDITED: " ++ question2WithChangeProperties ^. title
  , _editQuestionEventText = Nothing
  , _editQuestionEventAnswerIds = Nothing
  , _editQuestionEventExpertIds = Nothing
  , _editQuestionEventReferenceIds = Nothing
  }

e_km1_ch1_q2 :: EditQuestionEvent
e_km1_ch1_q2 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "f56b1435-ec9f-4d79-88b3-04c39b73724d"
  , _editQuestionEventKmUuid = km1 ^. uuid
  , _editQuestionEventChapterUuid = chapter1 ^. uuid
  , _editQuestionEventQuestionUuid = question2 ^. uuid
  , _editQuestionEventShortQuestionUuid = Just $ question2 ^. shortUuid
  , _editQuestionEventQType = Just $ question2WithChangeProperties ^. qType
  , _editQuestionEventTitle = Just $ question2WithChangeProperties ^. title
  , _editQuestionEventText = Just $ question2WithChangeProperties ^. text
  , _editQuestionEventAnswerIds = Just $ getAnwerIds question2WithChangeProperties
  , _editQuestionEventExpertIds = Just $ getExpertIds question2WithChangeProperties
  , _editQuestionEventReferenceIds = Just $ getReferenceIds question2WithChangeProperties
  }

e_km1_ch1_q2_second_edit :: EditQuestionEvent
e_km1_ch1_q2_second_edit =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "bf888b95-921d-4caa-88af-3309393d44c3"
  , _editQuestionEventKmUuid = km1 ^. uuid
  , _editQuestionEventChapterUuid = chapter1 ^. uuid
  , _editQuestionEventQuestionUuid = question2 ^. uuid
  , _editQuestionEventShortQuestionUuid = Just $ question2 ^. shortUuid
  , _editQuestionEventQType = Just $ question2WithChangeProperties ^. qType
  , _editQuestionEventTitle = Just "New title"
  , _editQuestionEventText = Just $ question2WithChangeProperties ^. text
  , _editQuestionEventAnswerIds = Just $ getAnwerIds question2WithChangeProperties
  , _editQuestionEventExpertIds = Just $ getExpertIds question2WithChangeProperties
  , _editQuestionEventReferenceIds = Just $ getReferenceIds question2WithChangeProperties
  }

d_km1_ch1_q1 :: DeleteQuestionEvent
d_km1_ch1_q1 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
  , _deleteQuestionEventKmUuid = km1 ^. uuid
  , _deleteQuestionEventChapterUuid = chapter1 ^. uuid
  , _deleteQuestionEventQuestionUuid = question1 ^. uuid
  }

d_km1_ch1_q1_2 :: DeleteQuestionEvent
d_km1_ch1_q1_2 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
  , _deleteQuestionEventKmUuid = km1 ^. uuid
  , _deleteQuestionEventChapterUuid = chapter1 ^. uuid
  , _deleteQuestionEventQuestionUuid = question1 ^. uuid
  }

d_km1_ch1_q2 :: DeleteQuestionEvent
d_km1_ch1_q2 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "52a7a6ae-be37-4075-ac5c-a20858707a75"
  , _deleteQuestionEventKmUuid = km1 ^. uuid
  , _deleteQuestionEventChapterUuid = chapter1 ^. uuid
  , _deleteQuestionEventQuestionUuid = question2 ^. uuid
  }

d_km1_ch1_q3 :: DeleteQuestionEvent
d_km1_ch1_q3 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "e46d208f-eb7d-48bc-8187-13a72b17ddb2"
  , _deleteQuestionEventKmUuid = km1 ^. uuid
  , _deleteQuestionEventChapterUuid = chapter1 ^. uuid
  , _deleteQuestionEventQuestionUuid = question3 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_aNo1 :: AddAnswerEvent
a_km1_ch1_q2_aNo1 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "afb36736-503a-43ca-a56b-8c144f89809e"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = question2 ^. uuid
  , _addAnswerEventAnswerUuid = answerNo1 ^. uuid
  , _addAnswerEventLabel = answerNo1 ^. label
  , _addAnswerEventAdvice = answerNo1 ^. advice
  }

a_km1_ch1_q2_aYes1 :: AddAnswerEvent
a_km1_ch1_q2_aYes1 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "e7ee93e4-18e7-4748-b0a5-781c77b8c937"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = question2 ^. uuid
  , _addAnswerEventAnswerUuid = answerYes1 ^. uuid
  , _addAnswerEventLabel = answerYes1 ^. label
  , _addAnswerEventAdvice = answerYes1 ^. advice
  }

a_km1_ch1_q2_aMaybe :: AddAnswerEvent
a_km1_ch1_q2_aMaybe =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "8ba60993-96ac-496b-9b8c-9580bf992cab"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = question2 ^. uuid
  , _addAnswerEventAnswerUuid = answerMaybe ^. uuid
  , _addAnswerEventLabel = answerMaybe ^. label
  , _addAnswerEventAdvice = answerMaybe ^. advice
  }

a_km1_ch1_q2_aNo3 :: AddAnswerEvent
a_km1_ch1_q2_aNo3 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "e62168e2-afe5-4e58-8ee7-555594aec23e"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = followUpQuestion1 ^. uuid
  , _addAnswerEventAnswerUuid = answerNo3 ^. uuid
  , _addAnswerEventLabel = answerNo3 ^. label
  , _addAnswerEventAdvice = answerNo3 ^. advice
  }

a_km1_ch1_q2_aYes3 :: AddAnswerEvent
a_km1_ch1_q2_aYes3 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "bc530681-b45b-4d36-b179-a9cb62a92838"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = followUpQuestion1 ^. uuid
  , _addAnswerEventAnswerUuid = answerYes3 ^. uuid
  , _addAnswerEventLabel = answerYes3 ^. label
  , _addAnswerEventAdvice = answerYes3 ^. advice
  }

a_km1_ch1_q2_aNo4 :: AddAnswerEvent
a_km1_ch1_q2_aNo4 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "abf67af9-23e0-43fa-a54a-746570882624"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = followUpQuestion2 ^. uuid
  , _addAnswerEventAnswerUuid = answerNo4 ^. uuid
  , _addAnswerEventLabel = answerNo4 ^. label
  , _addAnswerEventAdvice = answerNo4 ^. advice
  }

a_km1_ch1_q2_aYes4 :: AddAnswerEvent
a_km1_ch1_q2_aYes4 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "542c0d28-9ae3-4bbe-8030-92a78b462276"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = followUpQuestion2 ^. uuid
  , _addAnswerEventAnswerUuid = answerYes4 ^. uuid
  , _addAnswerEventLabel = answerYes4 ^. label
  , _addAnswerEventAdvice = answerYes4 ^. advice
  }

a_km1_ch2_q3_aNo2 :: AddAnswerEvent
a_km1_ch2_q3_aNo2 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "1bb10e82-33b5-4c98-b1d1-ab5413b5df66"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter2 ^. uuid
  , _addAnswerEventQuestionUuid = question3 ^. uuid
  , _addAnswerEventAnswerUuid = answerNo2 ^. uuid
  , _addAnswerEventLabel = answerNo2 ^. label
  , _addAnswerEventAdvice = answerNo2 ^. advice
  }

a_km1_ch2_q3_aYes2 :: AddAnswerEvent
a_km1_ch2_q3_aYes2 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "885ea1b9-0041-4240-911c-f35a9a6e4cbd"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter2 ^. uuid
  , _addAnswerEventQuestionUuid = question3 ^. uuid
  , _addAnswerEventAnswerUuid = answerYes2 ^. uuid
  , _addAnswerEventLabel = answerYes2 ^. label
  , _addAnswerEventAdvice = answerYes2 ^. advice
  }

e_km1_ch1_q2_aYes1 :: EditAnswerEvent
e_km1_ch1_q2_aYes1 =
  EditAnswerEvent
  { _editAnswerEventUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
  , _editAnswerEventKmUuid = km1 ^. uuid
  , _editAnswerEventChapterUuid = chapter1 ^. uuid
  , _editAnswerEventQuestionUuid = question2 ^. uuid
  , _editAnswerEventAnswerUuid = answerYes1 ^. uuid
  , _editAnswerEventLabel = Just $ answerYes1Changed ^. label
  , _editAnswerEventAdvice = Just $ answerYes1Changed ^. advice
  , _editAnswerEventFollowUpIds = Just $ getFollowUpIds answerYes1Changed
  }

e_km1_ch1_q2_aYes1_2 :: EditAnswerEvent
e_km1_ch1_q2_aYes1_2 =
  EditAnswerEvent
  { _editAnswerEventUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
  , _editAnswerEventKmUuid = km1 ^. uuid
  , _editAnswerEventChapterUuid = chapter1 ^. uuid
  , _editAnswerEventQuestionUuid = question2 ^. uuid
  , _editAnswerEventAnswerUuid = answerYes1 ^. uuid
  , _editAnswerEventLabel = Just $ answerYes1Changed ^. label
  , _editAnswerEventAdvice = Just $ answerYes1Changed ^. advice
  , _editAnswerEventFollowUpIds = Just $ getFollowUpIds answerYes1
  }

d_km1_ch1_q2_aYes1 :: DeleteAnswerEvent
d_km1_ch1_q2_aYes1 =
  DeleteAnswerEvent
  { _deleteAnswerEventUuid = fromJust $ U.fromString "1968692f-959a-4d47-b85f-d684eedb3e7f"
  , _deleteAnswerEventKmUuid = km1 ^. uuid
  , _deleteAnswerEventChapterUuid = chapter1 ^. uuid
  , _deleteAnswerEventQuestionUuid = question2 ^. uuid
  , _deleteAnswerEventAnswerUuid = answerYes1 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_ansYes1_fuq1 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1 =
  AddFollowUpQuestionEvent
  { _addFollowUpQuestionEventUuid = fromJust $ U.fromString "3588358c-159e-41a9-9847-262611007b61"
  , _addFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _addFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _addFollowUpQuestionEventAnswerUuid = answerYes1 ^. uuid
  , _addFollowUpQuestionEventQuestionUuid = followUpQuestion1 ^. uuid
  , _addFollowUpQuestionEventShortQuestionUuid = followUpQuestion1 ^. shortUuid
  , _addFollowUpQuestionEventQType = followUpQuestion1 ^. qType
  , _addFollowUpQuestionEventTitle = followUpQuestion1 ^. title
  , _addFollowUpQuestionEventText = followUpQuestion1 ^. text
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  AddFollowUpQuestionEvent
  { _addFollowUpQuestionEventUuid = fromJust $ U.fromString "8ced5634-a879-4da2-b7c9-158ca6a4e0e3"
  , _addFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _addFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _addFollowUpQuestionEventAnswerUuid = answerYes3 ^. uuid
  , _addFollowUpQuestionEventQuestionUuid = followUpQuestion2 ^. uuid
  , _addFollowUpQuestionEventShortQuestionUuid = followUpQuestion2 ^. shortUuid
  , _addFollowUpQuestionEventQType = followUpQuestion2 ^. qType
  , _addFollowUpQuestionEventTitle = followUpQuestion2 ^. title
  , _addFollowUpQuestionEventText = followUpQuestion2 ^. text
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3 =
  AddFollowUpQuestionEvent
  { _addFollowUpQuestionEventUuid = fromJust $ U.fromString "6e9b591f-e6f9-46dd-85e8-a90fe4acc51c"
  , _addFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _addFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _addFollowUpQuestionEventAnswerUuid = answerYes4 ^. uuid
  , _addFollowUpQuestionEventQuestionUuid = followUpQuestion3 ^. uuid
  , _addFollowUpQuestionEventShortQuestionUuid = followUpQuestion3 ^. shortUuid
  , _addFollowUpQuestionEventQType = followUpQuestion3 ^. qType
  , _addFollowUpQuestionEventTitle = followUpQuestion3 ^. title
  , _addFollowUpQuestionEventText = followUpQuestion3 ^. text
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: EditFollowUpQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  EditFollowUpQuestionEvent
  { _editFollowUpQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _editFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _editFollowUpQuestionEventAnswerUuid = answerYes3 ^. uuid
  , _editFollowUpQuestionEventQuestionUuid = followUpQuestion2 ^. uuid
  , _editFollowUpQuestionEventShortQuestionUuid = Just $ followUpQuestion2 ^. shortUuid
  , _editFollowUpQuestionEventQType = Just $ followUpQuestion2Changed ^. qType
  , _editFollowUpQuestionEventTitle = Just $ followUpQuestion2Changed ^. title
  , _editFollowUpQuestionEventText = Just $ followUpQuestion2Changed ^. text
  , _editFollowUpQuestionEventAnswerIds = Just $ getAnwerIds followUpQuestion2Changed
  , _editFollowUpQuestionEventExpertIds = Just $ getExpertIds followUpQuestion2Changed
  , _editFollowUpQuestionEventReferenceIds = Just $ getReferenceIds followUpQuestion2Changed
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 :: EditFollowUpQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 =
  EditFollowUpQuestionEvent
  { _editFollowUpQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _editFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _editFollowUpQuestionEventAnswerUuid = answerYes3 ^. uuid
  , _editFollowUpQuestionEventQuestionUuid = followUpQuestion2 ^. uuid
  , _editFollowUpQuestionEventShortQuestionUuid = Just $ followUpQuestion2 ^. shortUuid
  , _editFollowUpQuestionEventQType = Just $ followUpQuestion2Changed ^. qType
  , _editFollowUpQuestionEventTitle = Just $ followUpQuestion2Changed ^. title
  , _editFollowUpQuestionEventText = Just $ followUpQuestion2Changed ^. text
  , _editFollowUpQuestionEventAnswerIds = Just $ [answerYes4 ^. uuid, answerNo4 ^. uuid]
  , _editFollowUpQuestionEventExpertIds = Just $ getExpertIds followUpQuestion2
  , _editFollowUpQuestionEventReferenceIds = Just $ getReferenceIds followUpQuestion2
  }

d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: DeleteFollowUpQuestionEvent
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  DeleteFollowUpQuestionEvent
  { _deleteFollowUpQuestionEventUuid = fromJust $ U.fromString "db69d694-cfb6-4461-8a13-81c01638f348"
  , _deleteFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _deleteFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _deleteFollowUpQuestionEventAnswerUuid = answerYes3 ^. uuid
  , _deleteFollowUpQuestionEventQuestionUuid = followUpQuestion2 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_eDarth :: AddExpertEvent
a_km1_ch1_q2_eDarth =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "ec76054f-d059-4a5f-81c9-1817004a913c"
  , _addExpertEventKmUuid = km1 ^. uuid
  , _addExpertEventChapterUuid = chapter1 ^. uuid
  , _addExpertEventQuestionUuid = question2 ^. uuid
  , _addExpertEventExpertUuid = expertDarth ^. uuid
  , _addExpertEventName = expertDarth ^. name
  , _addExpertEventEmail = expertDarth ^. email
  }

a_km1_ch1_q2_eLuke :: AddExpertEvent
a_km1_ch1_q2_eLuke =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "40bb45bd-4195-4430-ac8f-16ac5a61ece0"
  , _addExpertEventKmUuid = km1 ^. uuid
  , _addExpertEventChapterUuid = chapter1 ^. uuid
  , _addExpertEventQuestionUuid = question2 ^. uuid
  , _addExpertEventExpertUuid = expertLuke ^. uuid
  , _addExpertEventName = expertLuke ^. name
  , _addExpertEventEmail = expertLuke ^. email
  }

a_km1_ch1_q2_eJohn :: AddExpertEvent
a_km1_ch1_q2_eJohn =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "2d5eedae-1782-44ac-9d4e-3db769161448"
  , _addExpertEventKmUuid = km1 ^. uuid
  , _addExpertEventChapterUuid = chapter1 ^. uuid
  , _addExpertEventQuestionUuid = question2 ^. uuid
  , _addExpertEventExpertUuid = expertJohn ^. uuid
  , _addExpertEventName = expertJohn ^. name
  , _addExpertEventEmail = expertJohn ^. email
  }

e_km1_ch1_q2_eDarth :: EditExpertEvent
e_km1_ch1_q2_eDarth =
  EditExpertEvent
  { _editExpertEventUuid = fromJust $ U.fromString "01686131-2423-4d97-a949-4fea2c9ce3b7"
  , _editExpertEventKmUuid = km1 ^. uuid
  , _editExpertEventChapterUuid = chapter1 ^. uuid
  , _editExpertEventQuestionUuid = question2 ^. uuid
  , _editExpertEventExpertUuid = expertDarth ^. uuid
  , _editExpertEventName = Just $ expertDarthChanged ^. name
  , _editExpertEventEmail = Just $ expertDarthChanged ^. email
  }

d_km1_ch1_q2_eLuke :: DeleteExpertEvent
d_km1_ch1_q2_eLuke =
  DeleteExpertEvent
  { _deleteExpertEventUuid = fromJust $ U.fromString "f20bc988-6d44-4051-990d-d16b24f369ac"
  , _deleteExpertEventKmUuid = km1 ^. uuid
  , _deleteExpertEventChapterUuid = chapter1 ^. uuid
  , _deleteExpertEventQuestionUuid = question2 ^. uuid
  , _deleteExpertEventExpertUuid = expertLuke ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_rCh1 :: AddReferenceEvent
a_km1_ch1_q2_rCh1 =
  AddReferenceEvent
  { _addReferenceEventUuid = fromJust $ U.fromString "1177d72f-b7d8-466d-ad33-d5f82d0f192a"
  , _addReferenceEventKmUuid = km1 ^. uuid
  , _addReferenceEventChapterUuid = chapter1 ^. uuid
  , _addReferenceEventQuestionUuid = question2 ^. uuid
  , _addReferenceEventReferenceUuid = referenceCh1 ^. uuid
  , _addReferenceEventChapter = referenceCh1 ^. chapter
  }

a_km1_ch1_q2_rCh2 :: AddReferenceEvent
a_km1_ch1_q2_rCh2 =
  AddReferenceEvent
  { _addReferenceEventUuid = fromJust $ U.fromString "4814f50f-8838-4b53-8b18-c0f8c568220e"
  , _addReferenceEventKmUuid = km1 ^. uuid
  , _addReferenceEventChapterUuid = chapter1 ^. uuid
  , _addReferenceEventQuestionUuid = question2 ^. uuid
  , _addReferenceEventReferenceUuid = referenceCh2 ^. uuid
  , _addReferenceEventChapter = referenceCh2 ^. chapter
  }

a_km1_ch1_q2_rCh3 :: AddReferenceEvent
a_km1_ch1_q2_rCh3 =
  AddReferenceEvent
  { _addReferenceEventUuid = fromJust $ U.fromString "45d8ec86-34bc-4e8f-b42a-48a567a77d8b"
  , _addReferenceEventKmUuid = km1 ^. uuid
  , _addReferenceEventChapterUuid = chapter1 ^. uuid
  , _addReferenceEventQuestionUuid = question2 ^. uuid
  , _addReferenceEventReferenceUuid = referenceCh3 ^. uuid
  , _addReferenceEventChapter = referenceCh3 ^. chapter
  }

e_km1_ch1_q2_rCh1 :: EditReferenceEvent
e_km1_ch1_q2_rCh1 =
  EditReferenceEvent
  { _editReferenceEventUuid = fromJust $ U.fromString "08cd9afc-d416-48ab-8669-17e87ceb15dc"
  , _editReferenceEventKmUuid = km1 ^. uuid
  , _editReferenceEventChapterUuid = chapter1 ^. uuid
  , _editReferenceEventQuestionUuid = question2 ^. uuid
  , _editReferenceEventReferenceUuid = referenceCh1 ^. uuid
  , _editReferenceEventChapter = Just $ referenceCh1Changed ^. chapter
  }

d_km1_ch1_q2_rCh2 :: DeleteReferenceEvent
d_km1_ch1_q2_rCh2 =
  DeleteReferenceEvent
  { _deleteReferenceEventUuid = fromJust $ U.fromString "3cc15f31-4801-404f-ba48-6b91f77d1abe"
  , _deleteReferenceEventKmUuid = km1 ^. uuid
  , _deleteReferenceEventChapterUuid = chapter1 ^. uuid
  , _deleteReferenceEventQuestionUuid = question2 ^. uuid
  , _deleteReferenceEventReferenceUuid = referenceCh2 ^. uuid
  }
