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
import Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEvent
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
  , _editKnowledgeModelEventName = ChangedValue $ km1WithChangeProperties ^. name
  , _editKnowledgeModelEventChapterIds = ChangedValue $ getChapterIds km1WithChangeProperties
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
  , _editChapterEventTitle = ChangedValue $ chapter1WithChangeProperties ^. title
  , _editChapterEventText = ChangedValue $ chapter1WithChangeProperties ^. text
  , _editChapterEventQuestionIds = ChangedValue $ getQuestionIds chapter1WithChangeProperties
  }

e_km1_ch1_2 :: EditChapterEvent
e_km1_ch1_2 =
  EditChapterEvent
  { _editChapterEventUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _editChapterEventKmUuid = km1 ^. uuid
  , _editChapterEventChapterUuid = chapter1 ^. uuid
  , _editChapterEventTitle = ChangedValue $ "TWICE: " ++ chapter1WithChangeProperties ^. title
  , _editChapterEventText = ChangedValue $ chapter1WithChangeProperties ^. text
  , _editChapterEventQuestionIds = ChangedValue $ getQuestionIds chapter1WithChangeProperties
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
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
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
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
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
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
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
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4 :: AddQuestionEvent
a_km1_ch2_q4 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
  , _addQuestionEventKmUuid = km1 ^. uuid
  , _addQuestionEventChapterUuid = chapter2 ^. uuid
  , _addQuestionEventQuestionUuid = question4 ^. uuid
  , _addQuestionEventShortQuestionUuid = question4 ^. shortUuid
  , _addQuestionEventQType = question4 ^. qType
  , _addQuestionEventTitle = question4 ^. title
  , _addQuestionEventText = question4 ^. text
  , _addQuestionEventAnswerItemTemplatePlain =
      Just
        AnswerItemTemplatePlain {_answerItemTemplatePlainTitle = (fromJust $ question4 ^. answerItemTemplate) ^. title}
  }

e_km1_ch1_q1_title :: EditQuestionEvent
e_km1_ch1_q1_title =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "de86f82b-aaaf-482e-97c7-c7e93d834cd9"
  , _editQuestionEventKmUuid = km1 ^. uuid
  , _editQuestionEventChapterUuid = chapter1 ^. uuid
  , _editQuestionEventQuestionUuid = question1 ^. uuid
  , _editQuestionEventShortQuestionUuid = NothingChanged
  , _editQuestionEventQType = NothingChanged
  , _editQuestionEventTitle = ChangedValue $ "EDITED: " ++ question2WithChangeProperties ^. title
  , _editQuestionEventText = NothingChanged
  , _editQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editQuestionEventAnswerIds = NothingChanged
  , _editQuestionEventExpertIds = NothingChanged
  , _editQuestionEventReferenceIds = NothingChanged
  }

e_km1_ch1_q2 :: EditQuestionEvent
e_km1_ch1_q2 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "f56b1435-ec9f-4d79-88b3-04c39b73724d"
  , _editQuestionEventKmUuid = km1 ^. uuid
  , _editQuestionEventChapterUuid = chapter1 ^. uuid
  , _editQuestionEventQuestionUuid = question2 ^. uuid
  , _editQuestionEventShortQuestionUuid = ChangedValue $ question2 ^. shortUuid
  , _editQuestionEventQType = ChangedValue $ question2WithChangeProperties ^. qType
  , _editQuestionEventTitle = ChangedValue $ question2WithChangeProperties ^. title
  , _editQuestionEventText = ChangedValue $ question2WithChangeProperties ^. text
  , _editQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editQuestionEventAnswerIds = ChangedValue $ getAnwerIds question2WithChangeProperties
  , _editQuestionEventExpertIds = ChangedValue $ getExpertIds question2WithChangeProperties
  , _editQuestionEventReferenceIds = ChangedValue $ getReferenceIds question2WithChangeProperties
  }

e_km1_ch1_q2_second_edit :: EditQuestionEvent
e_km1_ch1_q2_second_edit =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "bf888b95-921d-4caa-88af-3309393d44c3"
  , _editQuestionEventKmUuid = km1 ^. uuid
  , _editQuestionEventChapterUuid = chapter1 ^. uuid
  , _editQuestionEventQuestionUuid = question2 ^. uuid
  , _editQuestionEventShortQuestionUuid = ChangedValue $ question2 ^. shortUuid
  , _editQuestionEventQType = ChangedValue $ question2WithChangeProperties ^. qType
  , _editQuestionEventTitle = ChangedValue "New title"
  , _editQuestionEventText = ChangedValue $ question2WithChangeProperties ^. text
  , _editQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editQuestionEventAnswerIds = ChangedValue $ getAnwerIds question2WithChangeProperties
  , _editQuestionEventExpertIds = ChangedValue $ getExpertIds question2WithChangeProperties
  , _editQuestionEventReferenceIds = ChangedValue $ getReferenceIds question2WithChangeProperties
  }

e_km1_ch2_q4 :: EditQuestionEvent
e_km1_ch2_q4 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "bf888b95-921d-4caa-88af-3309393d44c3"
  , _editQuestionEventKmUuid = km1 ^. uuid
  , _editQuestionEventChapterUuid = chapter1 ^. uuid
  , _editQuestionEventQuestionUuid = question4WithChangeProperties ^. uuid
  , _editQuestionEventShortQuestionUuid = ChangedValue $ question4WithChangeProperties ^. shortUuid
  , _editQuestionEventQType = ChangedValue $ question4WithChangeProperties ^. qType
  , _editQuestionEventTitle = ChangedValue $ question4WithChangeProperties ^. title
  , _editQuestionEventText = ChangedValue $ question4WithChangeProperties ^. text
  , _editQuestionEventAnswerItemTemplatePlainWithIds =
      ChangedValue . Just $
      AnswerItemTemplatePlainWithIds
      { _answerItemTemplatePlainWithIdsTitle = (fromJust $ question4WithChangeProperties ^. answerItemTemplate) ^. title
      , _answerItemTemplatePlainWithIdsQuestionIds =
          (fromJust $ question4WithChangeProperties ^. answerItemTemplate) ^.. questions . traverse . uuid
      }
  , _editQuestionEventAnswerIds = ChangedValue $ getAnwerIds question4WithChangeProperties
  , _editQuestionEventExpertIds = ChangedValue $ getExpertIds question4WithChangeProperties
  , _editQuestionEventReferenceIds = ChangedValue $ getReferenceIds question4WithChangeProperties
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

a_km1_ch1_q2_aNoFu1 :: AddAnswerEvent
a_km1_ch1_q2_aNoFu1 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "e62168e2-afe5-4e58-8ee7-555594aec23e"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = followUpQuestion1 ^. uuid
  , _addAnswerEventAnswerUuid = answerNoFuq1 ^. uuid
  , _addAnswerEventLabel = answerNoFuq1 ^. label
  , _addAnswerEventAdvice = answerNoFuq1 ^. advice
  }

a_km1_ch1_q2_aYesFu1 :: AddAnswerEvent
a_km1_ch1_q2_aYesFu1 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "bc530681-b45b-4d36-b179-a9cb62a92838"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = followUpQuestion1 ^. uuid
  , _addAnswerEventAnswerUuid = answerYesFuq1 ^. uuid
  , _addAnswerEventLabel = answerYesFuq1 ^. label
  , _addAnswerEventAdvice = answerYesFuq1 ^. advice
  }

a_km1_ch1_q2_aNoFu2 :: AddAnswerEvent
a_km1_ch1_q2_aNoFu2 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "abf67af9-23e0-43fa-a54a-746570882624"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = followUpQuestion2 ^. uuid
  , _addAnswerEventAnswerUuid = answerNoFuq2 ^. uuid
  , _addAnswerEventLabel = answerNoFuq2 ^. label
  , _addAnswerEventAdvice = answerNoFuq2 ^. advice
  }

a_km1_ch1_q2_aYesFu2 :: AddAnswerEvent
a_km1_ch1_q2_aYesFu2 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "542c0d28-9ae3-4bbe-8030-92a78b462276"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter1 ^. uuid
  , _addAnswerEventQuestionUuid = followUpQuestion2 ^. uuid
  , _addAnswerEventAnswerUuid = answerYesFuq2 ^. uuid
  , _addAnswerEventLabel = answerYesFuq2 ^. label
  , _addAnswerEventAdvice = answerYesFuq2 ^. advice
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

a_km1_ch2_q6_aNo6 :: AddAnswerEvent
a_km1_ch2_q6_aNo6 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "c0a67ce5-21b3-47c7-8624-c2da26fb494f"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter2 ^. uuid
  , _addAnswerEventQuestionUuid = question6 ^. uuid
  , _addAnswerEventAnswerUuid = answerNo6 ^. uuid
  , _addAnswerEventLabel = answerNo6 ^. label
  , _addAnswerEventAdvice = answerNo6 ^. advice
  }

a_km1_ch2_q6_aYes6 :: AddAnswerEvent
a_km1_ch2_q6_aYes6 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "c5c42f99-613b-4b6c-ae5e-af784f51c483"
  , _addAnswerEventKmUuid = km1 ^. uuid
  , _addAnswerEventChapterUuid = chapter2 ^. uuid
  , _addAnswerEventQuestionUuid = question6 ^. uuid
  , _addAnswerEventAnswerUuid = answerYes6 ^. uuid
  , _addAnswerEventLabel = answerYes6 ^. label
  , _addAnswerEventAdvice = answerYes6 ^. advice
  }

e_km1_ch1_q2_aYes1 :: EditAnswerEvent
e_km1_ch1_q2_aYes1 =
  EditAnswerEvent
  { _editAnswerEventUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
  , _editAnswerEventKmUuid = km1 ^. uuid
  , _editAnswerEventChapterUuid = chapter1 ^. uuid
  , _editAnswerEventQuestionUuid = question2 ^. uuid
  , _editAnswerEventAnswerUuid = answerYes1 ^. uuid
  , _editAnswerEventLabel = ChangedValue $ answerYes1Changed ^. label
  , _editAnswerEventAdvice = ChangedValue $ answerYes1Changed ^. advice
  , _editAnswerEventFollowUpIds = ChangedValue $ getFollowUpIds answerYes1Changed
  }

e_km1_ch1_q2_aYes1_2 :: EditAnswerEvent
e_km1_ch1_q2_aYes1_2 =
  EditAnswerEvent
  { _editAnswerEventUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
  , _editAnswerEventKmUuid = km1 ^. uuid
  , _editAnswerEventChapterUuid = chapter1 ^. uuid
  , _editAnswerEventQuestionUuid = question2 ^. uuid
  , _editAnswerEventAnswerUuid = answerYes1 ^. uuid
  , _editAnswerEventLabel = ChangedValue $ answerYes1Changed ^. label
  , _editAnswerEventAdvice = ChangedValue $ answerYes1Changed ^. advice
  , _editAnswerEventFollowUpIds = ChangedValue $ getFollowUpIds answerYes1
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
a_km1_ch2_q4_ait1_q5 :: AddAnswerItemTemplateQuestionEvent
a_km1_ch2_q4_ait1_q5 =
  AddAnswerItemTemplateQuestionEvent
  { _addAnswerItemTemplateQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _addAnswerItemTemplateQuestionEventKmUuid = km1 ^. uuid
  , _addAnswerItemTemplateQuestionEventChapterUuid = chapter2 ^. uuid
  , _addAnswerItemTemplateQuestionEventParentQuestionUuid = question4 ^. uuid
  , _addAnswerItemTemplateQuestionEventQuestionUuid = question5 ^. uuid
  , _addAnswerItemTemplateQuestionEventShortQuestionUuid = question5 ^. shortUuid
  , _addAnswerItemTemplateQuestionEventQType = question5 ^. qType
  , _addAnswerItemTemplateQuestionEventTitle = question5 ^. title
  , _addAnswerItemTemplateQuestionEventText = question5 ^. text
  , _addAnswerItemTemplateQuestionEventAnswerItemTemplatePlain =
      Just
        AnswerItemTemplatePlain {_answerItemTemplatePlainTitle = (fromJust $ question5 ^. answerItemTemplate) ^. title}
  }

a_km1_ch2_q4_ait1_q6 :: AddAnswerItemTemplateQuestionEvent
a_km1_ch2_q4_ait1_q6 =
  AddAnswerItemTemplateQuestionEvent
  { _addAnswerItemTemplateQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _addAnswerItemTemplateQuestionEventKmUuid = km1 ^. uuid
  , _addAnswerItemTemplateQuestionEventChapterUuid = chapter2 ^. uuid
  , _addAnswerItemTemplateQuestionEventParentQuestionUuid = question4 ^. uuid
  , _addAnswerItemTemplateQuestionEventQuestionUuid = question6 ^. uuid
  , _addAnswerItemTemplateQuestionEventShortQuestionUuid = question6 ^. shortUuid
  , _addAnswerItemTemplateQuestionEventQType = question6 ^. qType
  , _addAnswerItemTemplateQuestionEventTitle = question6 ^. title
  , _addAnswerItemTemplateQuestionEventText = question6 ^. text
  , _addAnswerItemTemplateQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4_ait1_q6_fuq4_q1 :: AddAnswerItemTemplateQuestionEvent
a_km1_ch2_q4_ait1_q6_fuq4_q1 =
  AddAnswerItemTemplateQuestionEvent
  { _addAnswerItemTemplateQuestionEventUuid = fromJust $ U.fromString "55f46913-a953-4318-b72f-673e9f65fb2a"
  , _addAnswerItemTemplateQuestionEventKmUuid = km1 ^. uuid
  , _addAnswerItemTemplateQuestionEventChapterUuid = chapter2 ^. uuid
  , _addAnswerItemTemplateQuestionEventParentQuestionUuid = followUpQuestion4 ^. uuid
  , _addAnswerItemTemplateQuestionEventQuestionUuid = followUpQuestion4_question1 ^. uuid
  , _addAnswerItemTemplateQuestionEventShortQuestionUuid = followUpQuestion4_question1 ^. shortUuid
  , _addAnswerItemTemplateQuestionEventQType = followUpQuestion4_question1 ^. qType
  , _addAnswerItemTemplateQuestionEventTitle = followUpQuestion4_question1 ^. title
  , _addAnswerItemTemplateQuestionEventText = followUpQuestion4_question1 ^. text
  , _addAnswerItemTemplateQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4_ait1_q6_fuq4_q2 :: AddAnswerItemTemplateQuestionEvent
a_km1_ch2_q4_ait1_q6_fuq4_q2 =
  AddAnswerItemTemplateQuestionEvent
  { _addAnswerItemTemplateQuestionEventUuid = fromJust $ U.fromString "6b9a7c1c-a23e-458a-a1bb-d7500c0ed96e"
  , _addAnswerItemTemplateQuestionEventKmUuid = km1 ^. uuid
  , _addAnswerItemTemplateQuestionEventChapterUuid = chapter2 ^. uuid
  , _addAnswerItemTemplateQuestionEventParentQuestionUuid = followUpQuestion4 ^. uuid
  , _addAnswerItemTemplateQuestionEventQuestionUuid = followUpQuestion4_question2 ^. uuid
  , _addAnswerItemTemplateQuestionEventShortQuestionUuid = followUpQuestion4_question2 ^. shortUuid
  , _addAnswerItemTemplateQuestionEventQType = followUpQuestion4_question2 ^. qType
  , _addAnswerItemTemplateQuestionEventTitle = followUpQuestion4_question2 ^. title
  , _addAnswerItemTemplateQuestionEventText = followUpQuestion4_question2 ^. text
  , _addAnswerItemTemplateQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4_ait1_q7 :: AddAnswerItemTemplateQuestionEvent
a_km1_ch2_q4_ait1_q7 =
  AddAnswerItemTemplateQuestionEvent
  { _addAnswerItemTemplateQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _addAnswerItemTemplateQuestionEventKmUuid = km1 ^. uuid
  , _addAnswerItemTemplateQuestionEventChapterUuid = chapter2 ^. uuid
  , _addAnswerItemTemplateQuestionEventParentQuestionUuid = question5 ^. uuid
  , _addAnswerItemTemplateQuestionEventQuestionUuid = question7 ^. uuid
  , _addAnswerItemTemplateQuestionEventShortQuestionUuid = question7 ^. shortUuid
  , _addAnswerItemTemplateQuestionEventQType = question7 ^. qType
  , _addAnswerItemTemplateQuestionEventTitle = question7 ^. title
  , _addAnswerItemTemplateQuestionEventText = question7 ^. text
  , _addAnswerItemTemplateQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4_ait1_q8 :: AddAnswerItemTemplateQuestionEvent
a_km1_ch2_q4_ait1_q8 =
  AddAnswerItemTemplateQuestionEvent
  { _addAnswerItemTemplateQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _addAnswerItemTemplateQuestionEventKmUuid = km1 ^. uuid
  , _addAnswerItemTemplateQuestionEventChapterUuid = chapter2 ^. uuid
  , _addAnswerItemTemplateQuestionEventParentQuestionUuid = question5 ^. uuid
  , _addAnswerItemTemplateQuestionEventQuestionUuid = question8 ^. uuid
  , _addAnswerItemTemplateQuestionEventShortQuestionUuid = question8 ^. shortUuid
  , _addAnswerItemTemplateQuestionEventQType = question8 ^. qType
  , _addAnswerItemTemplateQuestionEventTitle = question8 ^. title
  , _addAnswerItemTemplateQuestionEventText = question8 ^. text
  , _addAnswerItemTemplateQuestionEventAnswerItemTemplatePlain = Nothing
  }

e_km1_ch2_q4_ait1_q5 :: EditAnswerItemTemplateQuestionEvent
e_km1_ch2_q4_ait1_q5 =
  EditAnswerItemTemplateQuestionEvent
  { _editAnswerItemTemplateQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _editAnswerItemTemplateQuestionEventKmUuid = km1 ^. uuid
  , _editAnswerItemTemplateQuestionEventChapterUuid = chapter2 ^. uuid
  , _editAnswerItemTemplateQuestionEventParentQuestionUuid = question4 ^. uuid
  , _editAnswerItemTemplateQuestionEventQuestionUuid = question5WithChangeProperties ^. uuid
  , _editAnswerItemTemplateQuestionEventShortQuestionUuid = ChangedValue $ question5WithChangeProperties ^. shortUuid
  , _editAnswerItemTemplateQuestionEventQType = ChangedValue $ question5WithChangeProperties ^. qType
  , _editAnswerItemTemplateQuestionEventTitle = ChangedValue $ question5WithChangeProperties ^. title
  , _editAnswerItemTemplateQuestionEventText = ChangedValue $ question5WithChangeProperties ^. text
  , _editAnswerItemTemplateQuestionEventAnswerItemTemplatePlainWithIds =
      ChangedValue . Just $
      AnswerItemTemplatePlainWithIds
      { _answerItemTemplatePlainWithIdsTitle = "EDITED: Template Title 2"
      , _answerItemTemplatePlainWithIdsQuestionIds = [question8 ^. uuid, question7 ^. uuid]
      }
  , _editAnswerItemTemplateQuestionEventAnswerIds = NothingChanged
  , _editAnswerItemTemplateQuestionEventExpertIds = NothingChanged
  , _editAnswerItemTemplateQuestionEventReferenceIds = NothingChanged
  }

e_km1_ch2_q4_ait1_q6 :: EditAnswerItemTemplateQuestionEvent
e_km1_ch2_q4_ait1_q6 =
  EditAnswerItemTemplateQuestionEvent
  { _editAnswerItemTemplateQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _editAnswerItemTemplateQuestionEventKmUuid = km1 ^. uuid
  , _editAnswerItemTemplateQuestionEventChapterUuid = chapter2 ^. uuid
  , _editAnswerItemTemplateQuestionEventParentQuestionUuid = question4 ^. uuid
  , _editAnswerItemTemplateQuestionEventQuestionUuid = question6WithChangeProperties ^. uuid
  , _editAnswerItemTemplateQuestionEventShortQuestionUuid = ChangedValue $ question6WithChangeProperties ^. shortUuid
  , _editAnswerItemTemplateQuestionEventQType = ChangedValue $ question6WithChangeProperties ^. qType
  , _editAnswerItemTemplateQuestionEventTitle = ChangedValue $ question6WithChangeProperties ^. title
  , _editAnswerItemTemplateQuestionEventText = ChangedValue $ question6WithChangeProperties ^. text
  , _editAnswerItemTemplateQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editAnswerItemTemplateQuestionEventAnswerIds = ChangedValue $ getAnwerIds question6WithChangeProperties
  , _editAnswerItemTemplateQuestionEventExpertIds = ChangedValue $ getExpertIds question6WithChangeProperties
  , _editAnswerItemTemplateQuestionEventReferenceIds = ChangedValue $ getReferenceIds question6WithChangeProperties
  }

d_km1_ch2_q4_ait1 :: DeleteAnswerItemTemplateQuestionEvent
d_km1_ch2_q4_ait1 =
  DeleteAnswerItemTemplateQuestionEvent
  { _deleteAnswerItemTemplateQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _deleteAnswerItemTemplateQuestionEventKmUuid = km1 ^. uuid
  , _deleteAnswerItemTemplateQuestionEventChapterUuid = chapter2 ^. uuid
  , _deleteAnswerItemTemplateQuestionEventParentQuestionUuid = question4 ^. uuid
  , _deleteAnswerItemTemplateQuestionEventQuestionUuid = question5 ^. uuid
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
  , _addFollowUpQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  AddFollowUpQuestionEvent
  { _addFollowUpQuestionEventUuid = fromJust $ U.fromString "8ced5634-a879-4da2-b7c9-158ca6a4e0e3"
  , _addFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _addFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _addFollowUpQuestionEventAnswerUuid = answerYesFuq1 ^. uuid
  , _addFollowUpQuestionEventQuestionUuid = followUpQuestion2 ^. uuid
  , _addFollowUpQuestionEventShortQuestionUuid = followUpQuestion2 ^. shortUuid
  , _addFollowUpQuestionEventQType = followUpQuestion2 ^. qType
  , _addFollowUpQuestionEventTitle = followUpQuestion2 ^. title
  , _addFollowUpQuestionEventText = followUpQuestion2 ^. text
  , _addFollowUpQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3 =
  AddFollowUpQuestionEvent
  { _addFollowUpQuestionEventUuid = fromJust $ U.fromString "6e9b591f-e6f9-46dd-85e8-a90fe4acc51c"
  , _addFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _addFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _addFollowUpQuestionEventAnswerUuid = answerYesFuq2 ^. uuid
  , _addFollowUpQuestionEventQuestionUuid = followUpQuestion3 ^. uuid
  , _addFollowUpQuestionEventShortQuestionUuid = followUpQuestion3 ^. shortUuid
  , _addFollowUpQuestionEventQType = followUpQuestion3 ^. qType
  , _addFollowUpQuestionEventTitle = followUpQuestion3 ^. title
  , _addFollowUpQuestionEventText = followUpQuestion3 ^. text
  , _addFollowUpQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_ansYes6_fuq4 :: AddFollowUpQuestionEvent
a_km1_ch2_ansYes6_fuq4 =
  AddFollowUpQuestionEvent
  { _addFollowUpQuestionEventUuid = fromJust $ U.fromString "c626fd42-80b8-4fd2-a16b-d38eeb8262f1"
  , _addFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _addFollowUpQuestionEventChapterUuid = chapter2 ^. uuid
  , _addFollowUpQuestionEventAnswerUuid = answerYes6 ^. uuid
  , _addFollowUpQuestionEventQuestionUuid = followUpQuestion4 ^. uuid
  , _addFollowUpQuestionEventShortQuestionUuid = followUpQuestion4 ^. shortUuid
  , _addFollowUpQuestionEventQType = followUpQuestion4 ^. qType
  , _addFollowUpQuestionEventTitle = followUpQuestion4 ^. title
  , _addFollowUpQuestionEventText = followUpQuestion4 ^. text
  , _addFollowUpQuestionEventAnswerItemTemplatePlain =
      Just
        AnswerItemTemplatePlain
        {_answerItemTemplatePlainTitle = (fromJust $ followUpQuestion4 ^. answerItemTemplate) ^. title}
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: EditFollowUpQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  EditFollowUpQuestionEvent
  { _editFollowUpQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _editFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _editFollowUpQuestionEventAnswerUuid = answerYesFuq1 ^. uuid
  , _editFollowUpQuestionEventQuestionUuid = followUpQuestion2 ^. uuid
  , _editFollowUpQuestionEventShortQuestionUuid = ChangedValue $ followUpQuestion2 ^. shortUuid
  , _editFollowUpQuestionEventQType = ChangedValue $ followUpQuestion2Changed ^. qType
  , _editFollowUpQuestionEventTitle = ChangedValue $ followUpQuestion2Changed ^. title
  , _editFollowUpQuestionEventText = ChangedValue $ followUpQuestion2Changed ^. text
  , _editFollowUpQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editFollowUpQuestionEventAnswerIds = ChangedValue $ getAnwerIds followUpQuestion2Changed
  , _editFollowUpQuestionEventExpertIds = ChangedValue $ getExpertIds followUpQuestion2Changed
  , _editFollowUpQuestionEventReferenceIds = ChangedValue $ getReferenceIds followUpQuestion2Changed
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 :: EditFollowUpQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 =
  EditFollowUpQuestionEvent
  { _editFollowUpQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _editFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _editFollowUpQuestionEventAnswerUuid = answerYesFuq1 ^. uuid
  , _editFollowUpQuestionEventQuestionUuid = followUpQuestion2 ^. uuid
  , _editFollowUpQuestionEventShortQuestionUuid = ChangedValue $ followUpQuestion2 ^. shortUuid
  , _editFollowUpQuestionEventQType = ChangedValue $ followUpQuestion2Changed ^. qType
  , _editFollowUpQuestionEventTitle = ChangedValue $ followUpQuestion2Changed ^. title
  , _editFollowUpQuestionEventText = ChangedValue $ followUpQuestion2Changed ^. text
  , _editFollowUpQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editFollowUpQuestionEventAnswerIds = ChangedValue $ Just [answerYesFuq2 ^. uuid, answerNoFuq2 ^. uuid]
  , _editFollowUpQuestionEventExpertIds = ChangedValue $ getExpertIds followUpQuestion2
  , _editFollowUpQuestionEventReferenceIds = ChangedValue $ getReferenceIds followUpQuestion2
  }

e_km1_ch2_ansMaybe6_fuq4 :: EditFollowUpQuestionEvent
e_km1_ch2_ansMaybe6_fuq4 =
  EditFollowUpQuestionEvent
  { _editFollowUpQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _editFollowUpQuestionEventChapterUuid = chapter2 ^. uuid
  , _editFollowUpQuestionEventAnswerUuid = answerNo6 ^. uuid
  , _editFollowUpQuestionEventQuestionUuid = followUpQuestion4Changed ^. uuid
  , _editFollowUpQuestionEventShortQuestionUuid = ChangedValue $ followUpQuestion4Changed ^. shortUuid
  , _editFollowUpQuestionEventQType = ChangedValue $ followUpQuestion4Changed ^. qType
  , _editFollowUpQuestionEventTitle = ChangedValue $ followUpQuestion4Changed ^. title
  , _editFollowUpQuestionEventText = ChangedValue $ followUpQuestion4Changed ^. text
  , _editFollowUpQuestionEventAnswerItemTemplatePlainWithIds =
      ChangedValue . Just $
      AnswerItemTemplatePlainWithIds
      { _answerItemTemplatePlainWithIdsTitle = "EDITED: fup 4 template title"
      , _answerItemTemplatePlainWithIdsQuestionIds =
          [followUpQuestion4_question2 ^. uuid, followUpQuestion4_question1 ^. uuid]
      }
  , _editFollowUpQuestionEventAnswerIds = NothingChanged
  , _editFollowUpQuestionEventExpertIds = NothingChanged
  , _editFollowUpQuestionEventReferenceIds = NothingChanged
  }

d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: DeleteFollowUpQuestionEvent
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  DeleteFollowUpQuestionEvent
  { _deleteFollowUpQuestionEventUuid = fromJust $ U.fromString "db69d694-cfb6-4461-8a13-81c01638f348"
  , _deleteFollowUpQuestionEventKmUuid = km1 ^. uuid
  , _deleteFollowUpQuestionEventChapterUuid = chapter1 ^. uuid
  , _deleteFollowUpQuestionEventAnswerUuid = answerYesFuq1 ^. uuid
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

a_km1_ch2_q6_eDarth :: AddExpertEvent
a_km1_ch2_q6_eDarth =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "eb6bb073-ecba-4cd0-91a3-ff31d374601f"
  , _addExpertEventKmUuid = km1 ^. uuid
  , _addExpertEventChapterUuid = chapter2 ^. uuid
  , _addExpertEventQuestionUuid = question6 ^. uuid
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

a_km1_ch2_q6_eLuke :: AddExpertEvent
a_km1_ch2_q6_eLuke =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "53653d05-6d5a-4b76-bbc6-15ca8314ad69"
  , _addExpertEventKmUuid = km1 ^. uuid
  , _addExpertEventChapterUuid = chapter2 ^. uuid
  , _addExpertEventQuestionUuid = question6 ^. uuid
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
  , _editExpertEventName = ChangedValue $ expertDarthChanged ^. name
  , _editExpertEventEmail = ChangedValue $ expertDarthChanged ^. email
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

a_km1_ch2_q6_rCh1 :: AddReferenceEvent
a_km1_ch2_q6_rCh1 =
  AddReferenceEvent
  { _addReferenceEventUuid = fromJust $ U.fromString "a3f6ee9a-803f-4911-9566-734a6358913a"
  , _addReferenceEventKmUuid = km1 ^. uuid
  , _addReferenceEventChapterUuid = chapter2 ^. uuid
  , _addReferenceEventQuestionUuid = question6 ^. uuid
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

a_km1_ch2_q6_rCh2 :: AddReferenceEvent
a_km1_ch2_q6_rCh2 =
  AddReferenceEvent
  { _addReferenceEventUuid = fromJust $ U.fromString "a4ae3400-dd3c-41ab-b796-4bf9d0bdafe7"
  , _addReferenceEventKmUuid = km1 ^. uuid
  , _addReferenceEventChapterUuid = chapter2 ^. uuid
  , _addReferenceEventQuestionUuid = question6 ^. uuid
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
  , _editReferenceEventChapter = ChangedValue $ referenceCh1Changed ^. chapter
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
