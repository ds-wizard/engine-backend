module Database.Migration.Development.Event.Data.Events where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.Experts
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.KnowledgeModel.Data.References
import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.EventPath
import Model.Event.Expert.ExpertEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors

a_km1 :: AddKnowledgeModelEvent
a_km1 =
  AddKnowledgeModelEvent
  { _addKnowledgeModelEventUuid = fromJust $ U.fromString "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f"
  , _addKnowledgeModelEventPath = []
  , _addKnowledgeModelEventKmUuid = km1WithoutChapters ^. uuid
  , _addKnowledgeModelEventName = km1WithoutChapters ^. name
  }

e_km1 :: EditKnowledgeModelEvent
e_km1 =
  EditKnowledgeModelEvent
  { _editKnowledgeModelEventUuid = fromJust $ U.fromString "8294a55d-642d-416c-879b-5a42a4430c24"
  , _editKnowledgeModelEventPath = []
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
  , _addChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _addChapterEventChapterUuid = chapter1WithoutQuestions ^. uuid
  , _addChapterEventTitle = chapter1WithoutQuestions ^. title
  , _addChapterEventText = chapter1WithoutQuestions ^. text
  }

a_km1_ch2 :: AddChapterEvent
a_km1_ch2 =
  AddChapterEvent
  { _addChapterEventUuid = fromJust $ U.fromString "6c4bba6e-864b-4871-98ca-49ac7a3e5eb5"
  , _addChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _addChapterEventChapterUuid = chapter2WithoutQuestions ^. uuid
  , _addChapterEventTitle = chapter2WithoutQuestions ^. title
  , _addChapterEventText = chapter2WithoutQuestions ^. text
  }

a_km1_ch3 :: AddChapterEvent
a_km1_ch3 =
  AddChapterEvent
  { _addChapterEventUuid = fromJust $ U.fromString "6585a64d-c75b-47fc-a86e-e0c8e773528f"
  , _addChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _addChapterEventChapterUuid = chapter3WithoutQuestions ^. uuid
  , _addChapterEventTitle = chapter3WithoutQuestions ^. title
  , _addChapterEventText = chapter3WithoutQuestions ^. text
  }

e_km1_ch1 :: EditChapterEvent
e_km1_ch1 =
  EditChapterEvent
  { _editChapterEventUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _editChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _editChapterEventChapterUuid = chapter1 ^. uuid
  , _editChapterEventTitle = ChangedValue $ chapter1WithChangeProperties ^. title
  , _editChapterEventText = ChangedValue $ chapter1WithChangeProperties ^. text
  , _editChapterEventQuestionIds = ChangedValue $ getQuestionIds chapter1WithChangeProperties
  }

e_km1_ch1_2 :: EditChapterEvent
e_km1_ch1_2 =
  EditChapterEvent
  { _editChapterEventUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _editChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _editChapterEventChapterUuid = chapter1 ^. uuid
  , _editChapterEventTitle = ChangedValue $ "TWICE: " ++ chapter1WithChangeProperties ^. title
  , _editChapterEventText = ChangedValue $ chapter1WithChangeProperties ^. text
  , _editChapterEventQuestionIds = ChangedValue $ getQuestionIds chapter1WithChangeProperties
  }

d_km1_ch1 :: DeleteChapterEvent
d_km1_ch1 =
  DeleteChapterEvent
  { _deleteChapterEventUuid = fromJust $ U.fromString "d07cc69b-abd3-43ec-bce1-fe59899dbda3"
  , _deleteChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _deleteChapterEventChapterUuid = chapter1 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q1 :: AddQuestionEvent
a_km1_ch1_q1 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "71ae2ce9-553b-4ca2-a542-1bce04406c51"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addQuestionEventQuestionUuid = question1 ^. uuid
  , _addQuestionEventQType = question1 ^. qType
  , _addQuestionEventTitle = question1 ^. title
  , _addQuestionEventText = question1 ^. text
  , _addQuestionEventRequiredLevel = question1 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch1_q2 :: AddQuestionEvent
a_km1_ch1_q2 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "ced9be29-24af-4443-8f5f-e709791a8fe3"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addQuestionEventQuestionUuid = question2 ^. uuid
  , _addQuestionEventQType = question2 ^. qType
  , _addQuestionEventTitle = question2 ^. title
  , _addQuestionEventText = question2 ^. text
  , _addQuestionEventRequiredLevel = question2 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch1_q3 :: AddQuestionEvent
a_km1_ch1_q3 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "d559ac95-cc81-4502-a780-dbaee46f24bc"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addQuestionEventQuestionUuid = question3 ^. uuid
  , _addQuestionEventQType = question3 ^. qType
  , _addQuestionEventTitle = question3 ^. title
  , _addQuestionEventText = question3 ^. text
  , _addQuestionEventRequiredLevel = question3 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q3 :: AddQuestionEvent
a_km1_ch2_q3 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addQuestionEventQuestionUuid = question3 ^. uuid
  , _addQuestionEventQType = question3 ^. qType
  , _addQuestionEventTitle = question3 ^. title
  , _addQuestionEventText = question3 ^. text
  , _addQuestionEventRequiredLevel = question3 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4 :: AddQuestionEvent
a_km1_ch2_q4 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addQuestionEventQuestionUuid = question4 ^. uuid
  , _addQuestionEventQType = question4 ^. qType
  , _addQuestionEventTitle = question4 ^. title
  , _addQuestionEventText = question4 ^. text
  , _addQuestionEventRequiredLevel = question4 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain =
      Just
        AnswerItemTemplatePlain {_answerItemTemplatePlainTitle = (fromJust $ question4 ^. answerItemTemplate) ^. title}
  }

e_km1_ch1_q1_title :: EditQuestionEvent
e_km1_ch1_q1_title =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "de86f82b-aaaf-482e-97c7-c7e93d834cd9"
  , _editQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editQuestionEventQuestionUuid = question1 ^. uuid
  , _editQuestionEventQType = NothingChanged
  , _editQuestionEventTitle = ChangedValue $ "EDITED: " ++ question2WithChangeProperties ^. title
  , _editQuestionEventText = NothingChanged
  , _editQuestionEventRequiredLevel = NothingChanged
  , _editQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editQuestionEventAnswerIds = NothingChanged
  , _editQuestionEventExpertIds = NothingChanged
  , _editQuestionEventReferenceIds = NothingChanged
  }

e_km1_ch1_q2 :: EditQuestionEvent
e_km1_ch1_q2 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "f56b1435-ec9f-4d79-88b3-04c39b73724d"
  , _editQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editQuestionEventQuestionUuid = question2 ^. uuid
  , _editQuestionEventQType = ChangedValue $ question2WithChangeProperties ^. qType
  , _editQuestionEventTitle = ChangedValue $ question2WithChangeProperties ^. title
  , _editQuestionEventText = ChangedValue $ question2WithChangeProperties ^. text
  , _editQuestionEventRequiredLevel = ChangedValue $ question2WithChangeProperties ^. requiredLevel
  , _editQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editQuestionEventAnswerIds = ChangedValue $ getAnwerIds question2WithChangeProperties
  , _editQuestionEventExpertIds = ChangedValue $ getExpertIds question2WithChangeProperties
  , _editQuestionEventReferenceIds = ChangedValue $ getReferenceIds question2WithChangeProperties
  }

e_km1_ch1_q2_second_edit :: EditQuestionEvent
e_km1_ch1_q2_second_edit =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "bf888b95-921d-4caa-88af-3309393d44c3"
  , _editQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editQuestionEventQuestionUuid = question2 ^. uuid
  , _editQuestionEventQType = ChangedValue $ question2WithChangeProperties ^. qType
  , _editQuestionEventTitle = ChangedValue "New title"
  , _editQuestionEventText = ChangedValue $ question2WithChangeProperties ^. text
  , _editQuestionEventRequiredLevel = ChangedValue $ question2WithChangeProperties ^. requiredLevel
  , _editQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editQuestionEventAnswerIds = ChangedValue $ getAnwerIds question2WithChangeProperties
  , _editQuestionEventExpertIds = ChangedValue $ getExpertIds question2WithChangeProperties
  , _editQuestionEventReferenceIds = ChangedValue $ getReferenceIds question2WithChangeProperties
  }

e_km1_ch2_q4 :: EditQuestionEvent
e_km1_ch2_q4 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "bf888b95-921d-4caa-88af-3309393d44c3"
  , _editQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editQuestionEventQuestionUuid = question4WithChangeProperties ^. uuid
  , _editQuestionEventQType = ChangedValue $ question4WithChangeProperties ^. qType
  , _editQuestionEventTitle = ChangedValue $ question4WithChangeProperties ^. title
  , _editQuestionEventText = ChangedValue $ question4WithChangeProperties ^. text
  , _editQuestionEventRequiredLevel = ChangedValue $ question4WithChangeProperties ^. requiredLevel
  , _editQuestionEventAnswerItemTemplatePlainWithIds =
      ChangedValue . Just $
      AnswerItemTemplatePlainWithIds
      { _answerItemTemplatePlainWithIdsTitle = fromJust (question4WithChangeProperties ^. answerItemTemplate) ^. title
      , _answerItemTemplatePlainWithIdsQuestionIds =
          fromJust (question4WithChangeProperties ^. answerItemTemplate) ^.. questions . traverse . uuid
      }
  , _editQuestionEventAnswerIds = ChangedValue $ getAnwerIds question4WithChangeProperties
  , _editQuestionEventExpertIds = ChangedValue $ getExpertIds question4WithChangeProperties
  , _editQuestionEventReferenceIds = ChangedValue $ getReferenceIds question4WithChangeProperties
  }

d_km1_ch1_q1 :: DeleteQuestionEvent
d_km1_ch1_q1 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
  , _deleteQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _deleteQuestionEventQuestionUuid = question1 ^. uuid
  }

d_km1_ch1_q1_2 :: DeleteQuestionEvent
d_km1_ch1_q1_2 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
  , _deleteQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _deleteQuestionEventQuestionUuid = question1 ^. uuid
  }

d_km1_ch1_q2 :: DeleteQuestionEvent
d_km1_ch1_q2 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "52a7a6ae-be37-4075-ac5c-a20858707a75"
  , _deleteQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _deleteQuestionEventQuestionUuid = question2 ^. uuid
  }

d_km1_ch1_q3 :: DeleteQuestionEvent
d_km1_ch1_q3 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "e46d208f-eb7d-48bc-8187-13a72b17ddb2"
  , _deleteQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _deleteQuestionEventQuestionUuid = question3 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_aNo1 :: AddAnswerEvent
a_km1_ch1_q2_aNo1 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "afb36736-503a-43ca-a56b-8c144f89809e"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q2_answerNo ^. uuid
  , _addAnswerEventLabel = q2_answerNo ^. label
  , _addAnswerEventAdvice = q2_answerNo ^. advice
  , _addAnswerEventMetricMeasures = q2_answerNo ^. metricMeasures
  }

a_km1_ch1_q2_aYes1 :: AddAnswerEvent
a_km1_ch1_q2_aYes1 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "e7ee93e4-18e7-4748-b0a5-781c77b8c937"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q2_answerYes ^. uuid
  , _addAnswerEventLabel = q2_answerYes ^. label
  , _addAnswerEventAdvice = q2_answerYes ^. advice
  , _addAnswerEventMetricMeasures = q2_answerYes ^. metricMeasures
  }

a_km1_ch1_q2_aMaybe :: AddAnswerEvent
a_km1_ch1_q2_aMaybe =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "8ba60993-96ac-496b-9b8c-9580bf992cab"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q2_answerMaybe ^. uuid
  , _addAnswerEventLabel = q2_answerMaybe ^. label
  , _addAnswerEventAdvice = q2_answerMaybe ^. advice
  , _addAnswerEventMetricMeasures = q2_answerMaybe ^. metricMeasures
  }

a_km1_ch1_q2_aYes1_fuq1_aNo :: AddAnswerEvent
a_km1_ch1_q2_aYes1_fuq1_aNo =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "e62168e2-afe5-4e58-8ee7-555594aec23e"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q2_aYes_fuq1_answerNo ^. uuid
  , _addAnswerEventLabel = q2_aYes_fuq1_answerNo ^. label
  , _addAnswerEventAdvice = q2_aYes_fuq1_answerNo ^. advice
  , _addAnswerEventMetricMeasures = q2_aYes_fuq1_answerNo ^. metricMeasures
  }

a_km1_ch1_q2_aYesFu1 :: AddAnswerEvent
a_km1_ch1_q2_aYesFu1 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "bc530681-b45b-4d36-b179-a9cb62a92838"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q2_aYes_fuq1_answerYes ^. uuid
  , _addAnswerEventLabel = q2_aYes_fuq1_answerYes ^. label
  , _addAnswerEventAdvice = q2_aYes_fuq1_answerYes ^. advice
  , _addAnswerEventMetricMeasures = q2_aYes_fuq1_answerYes ^. metricMeasures
  }

a_km1_ch1_q2_aNoFu2 :: AddAnswerEvent
a_km1_ch1_q2_aNoFu2 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "abf67af9-23e0-43fa-a54a-746570882624"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid
  , _addAnswerEventLabel = q2_aYes_fuq1_aYes_fuq2_answerNo ^. label
  , _addAnswerEventAdvice = q2_aYes_fuq1_aYes_fuq2_answerNo ^. advice
  , _addAnswerEventMetricMeasures = q2_aYes_fuq1_aYes_fuq2_answerNo ^. metricMeasures
  }

a_km1_ch1_q2_aYesFu2 :: AddAnswerEvent
a_km1_ch1_q2_aYesFu2 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "542c0d28-9ae3-4bbe-8030-92a78b462276"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid
  , _addAnswerEventLabel = q2_aYes_fuq1_aYes_fuq2_answerYes ^. label
  , _addAnswerEventAdvice = q2_aYes_fuq1_aYes_fuq2_answerYes ^. advice
  , _addAnswerEventMetricMeasures = q2_aYes_fuq1_aYes_fuq2_answerYes ^. metricMeasures
  }

a_km1_ch2_q3_aNo2 :: AddAnswerEvent
a_km1_ch2_q3_aNo2 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "1bb10e82-33b5-4c98-b1d1-ab5413b5df66"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question3 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q3_answerNo ^. uuid
  , _addAnswerEventLabel = q3_answerNo ^. label
  , _addAnswerEventAdvice = q3_answerNo ^. advice
  , _addAnswerEventMetricMeasures = q3_answerNo ^. metricMeasures
  }

a_km1_ch2_q3_aYes2 :: AddAnswerEvent
a_km1_ch2_q3_aYes2 =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "885ea1b9-0041-4240-911c-f35a9a6e4cbd"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question3 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q3_answerYes ^. uuid
  , _addAnswerEventLabel = q3_answerYes ^. label
  , _addAnswerEventAdvice = q3_answerYes ^. advice
  , _addAnswerEventMetricMeasures = q3_answerYes ^. metricMeasures
  }

a_km1_ch2_q4_ait_q6_aNo :: AddAnswerEvent
a_km1_ch2_q4_ait_q6_aNo =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "c0a67ce5-21b3-47c7-8624-c2da26fb494f"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q4_ait1_q6_answerNo ^. uuid
  , _addAnswerEventLabel = q4_ait1_q6_answerNo ^. label
  , _addAnswerEventAdvice = q4_ait1_q6_answerNo ^. advice
  , _addAnswerEventMetricMeasures = q4_ait1_q6_answerNo ^. metricMeasures
  }

a_km1_ch2_q4_ait_q6_aYes :: AddAnswerEvent
a_km1_ch2_q4_ait_q6_aYes =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "c5c42f99-613b-4b6c-ae5e-af784f51c483"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q4_ait1_q6_answerYes ^. uuid
  , _addAnswerEventLabel = q4_ait1_q6_answerYes ^. label
  , _addAnswerEventAdvice = q4_ait1_q6_answerYes ^. advice
  , _addAnswerEventMetricMeasures = q4_ait1_q6_answerYes ^. metricMeasures
  }

e_km1_ch1_q2_aYes1 :: EditAnswerEvent
e_km1_ch1_q2_aYes1 =
  EditAnswerEvent
  { _editAnswerEventUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
  , _editAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editAnswerEventAnswerUuid = q2_answerYes ^. uuid
  , _editAnswerEventLabel = ChangedValue $ q2_answerYesChanged ^. label
  , _editAnswerEventAdvice = ChangedValue $ q2_answerYesChanged ^. advice
  , _editAnswerEventFollowUpIds = ChangedValue $ getFollowUpIds q2_answerYesChanged
  , _editAnswerEventMetricMeasures = ChangedValue $ q2_answerYesChanged ^. metricMeasures
  }

e_km1_ch1_q2_aYes1_2 :: EditAnswerEvent
e_km1_ch1_q2_aYes1_2 =
  EditAnswerEvent
  { _editAnswerEventUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
  , _editAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editAnswerEventAnswerUuid = q2_answerYes ^. uuid
  , _editAnswerEventLabel = ChangedValue $ q2_answerYesChanged ^. label
  , _editAnswerEventAdvice = ChangedValue $ q2_answerYesChanged ^. advice
  , _editAnswerEventFollowUpIds = ChangedValue $ getFollowUpIds q2_answerYes
  , _editAnswerEventMetricMeasures = ChangedValue $ q2_answerYes ^. metricMeasures
  }

d_km1_ch1_q2_aYes1 :: DeleteAnswerEvent
d_km1_ch1_q2_aYes1 =
  DeleteAnswerEvent
  { _deleteAnswerEventUuid = fromJust $ U.fromString "1968692f-959a-4d47-b85f-d684eedb3e7f"
  , _deleteAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _deleteAnswerEventAnswerUuid = q2_answerYes ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- AnswerItemTemplateQuestionEvent
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch2_q4_ait1_q5 :: AddQuestionEvent
a_km1_ch2_q4_ait1_q5 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addQuestionEventQuestionUuid = q4_ait1_question5 ^. uuid
  , _addQuestionEventQType = q4_ait1_question5 ^. qType
  , _addQuestionEventTitle = q4_ait1_question5 ^. title
  , _addQuestionEventText = q4_ait1_question5 ^. text
  , _addQuestionEventRequiredLevel = q4_ait1_question5 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain =
      Just
        AnswerItemTemplatePlain
        {_answerItemTemplatePlainTitle = fromJust (q4_ait1_question5 ^. answerItemTemplate) ^. title}
  }

a_km1_ch2_q4_ait1_q6 :: AddQuestionEvent
a_km1_ch2_q4_ait1_q6 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addQuestionEventQuestionUuid = q4_ait1_question6 ^. uuid
  , _addQuestionEventQType = q4_ait1_question6 ^. qType
  , _addQuestionEventTitle = q4_ait1_question6 ^. title
  , _addQuestionEventText = q4_ait1_question6 ^. text
  , _addQuestionEventRequiredLevel = q4_ait1_question6 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4_ait1_q6_fuq4_q1 :: AddQuestionEvent
a_km1_ch2_q4_ait1_q6_fuq4_q1 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "55f46913-a953-4318-b72f-673e9f65fb2a"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q4_ait1_q6_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        { _eventPathItemUuid = q4_ait1_q6_aYes_followUpQuestion4 ^. uuid
        , _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION
        }
      ]
  , _addQuestionEventQuestionUuid = q4_ait1_q6_aYes_fuq4_ait_question1 ^. uuid
  , _addQuestionEventQType = q4_ait1_q6_aYes_fuq4_ait_question1 ^. qType
  , _addQuestionEventTitle = q4_ait1_q6_aYes_fuq4_ait_question1 ^. title
  , _addQuestionEventText = q4_ait1_q6_aYes_fuq4_ait_question1 ^. text
  , _addQuestionEventRequiredLevel = q4_ait1_q6_aYes_fuq4_ait_question1 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4_ait1_q6_fuq4_q2 :: AddQuestionEvent
a_km1_ch2_q4_ait1_q6_fuq4_q2 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "6b9a7c1c-a23e-458a-a1bb-d7500c0ed96e"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q4_ait1_q6_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        { _eventPathItemUuid = q4_ait1_q6_aYes_followUpQuestion4 ^. uuid
        , _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION
        }
      ]
  , _addQuestionEventQuestionUuid = q4_ait1_q6_aYes_fuq4_ait_question2 ^. uuid
  , _addQuestionEventQType = q4_ait1_q6_aYes_fuq4_ait_question2 ^. qType
  , _addQuestionEventTitle = q4_ait1_q6_aYes_fuq4_ait_question2 ^. title
  , _addQuestionEventText = q4_ait1_q6_aYes_fuq4_ait_question2 ^. text
  , _addQuestionEventRequiredLevel = q4_ait1_q6_aYes_fuq4_ait_question2 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4_ait1_q7 :: AddQuestionEvent
a_km1_ch2_q4_ait1_q7 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question5 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addQuestionEventQuestionUuid = q4_ait1_q5_ait2_question7 ^. uuid
  , _addQuestionEventQType = q4_ait1_q5_ait2_question7 ^. qType
  , _addQuestionEventTitle = q4_ait1_q5_ait2_question7 ^. title
  , _addQuestionEventText = q4_ait1_q5_ait2_question7 ^. text
  , _addQuestionEventRequiredLevel = q4_ait1_q5_ait2_question7 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_q4_ait1_q8 :: AddQuestionEvent
a_km1_ch2_q4_ait1_q8 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question5 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addQuestionEventQuestionUuid = q4_ait1_q5_ait2_question8 ^. uuid
  , _addQuestionEventQType = q4_ait1_q5_ait2_question8 ^. qType
  , _addQuestionEventTitle = q4_ait1_q5_ait2_question8 ^. title
  , _addQuestionEventText = q4_ait1_q5_ait2_question8 ^. text
  , _addQuestionEventRequiredLevel = q4_ait1_q5_ait2_question8 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

e_km1_ch2_q4_ait1_q5 :: EditQuestionEvent
e_km1_ch2_q4_ait1_q5 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _editQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editQuestionEventQuestionUuid = q4_ait1_question5Changed ^. uuid
  , _editQuestionEventQType = ChangedValue $ q4_ait1_question5Changed ^. qType
  , _editQuestionEventTitle = ChangedValue $ q4_ait1_question5Changed ^. title
  , _editQuestionEventText = ChangedValue $ q4_ait1_question5Changed ^. text
  , _editQuestionEventRequiredLevel = ChangedValue $ q4_ait1_question5Changed ^. requiredLevel
  , _editQuestionEventAnswerItemTemplatePlainWithIds =
      ChangedValue . Just $
      AnswerItemTemplatePlainWithIds
      { _answerItemTemplatePlainWithIdsTitle = "EDITED: Template Title 2"
      , _answerItemTemplatePlainWithIdsQuestionIds =
          [q4_ait1_q5_ait2_question8 ^. uuid, q4_ait1_q5_ait2_question7 ^. uuid]
      }
  , _editQuestionEventAnswerIds = NothingChanged
  , _editQuestionEventExpertIds = NothingChanged
  , _editQuestionEventReferenceIds = NothingChanged
  }

e_km1_ch2_q4_ait1_q6 :: EditQuestionEvent
e_km1_ch2_q4_ait1_q6 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _editQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editQuestionEventQuestionUuid = q4_ait1_question6Changed ^. uuid
  , _editQuestionEventQType = ChangedValue $ q4_ait1_question6Changed ^. qType
  , _editQuestionEventTitle = ChangedValue $ q4_ait1_question6Changed ^. title
  , _editQuestionEventText = ChangedValue $ q4_ait1_question6Changed ^. text
  , _editQuestionEventRequiredLevel = ChangedValue $ q4_ait1_question6Changed ^. requiredLevel
  , _editQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editQuestionEventAnswerIds = ChangedValue $ getAnwerIds q4_ait1_question6Changed
  , _editQuestionEventExpertIds = ChangedValue $ getExpertIds q4_ait1_question6Changed
  , _editQuestionEventReferenceIds = ChangedValue $ getReferenceIds q4_ait1_question6Changed
  }

d_km1_ch2_q4_ait1_q5 :: DeleteQuestionEvent
d_km1_ch2_q4_ait1_q5 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "263bc255-4289-4ca8-9734-8b254ab45f6b"
  , _deleteQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _deleteQuestionEventQuestionUuid = q4_ait1_question5 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- FollowUpQuestionEvent
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_ansYes1_fuq1 :: AddQuestionEvent
a_km1_ch1_ansYes1_fuq1 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "3588358c-159e-41a9-9847-262611007b61"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _addQuestionEventQuestionUuid = q2_aYes_fuQuestion1 ^. uuid
  , _addQuestionEventQType = q2_aYes_fuQuestion1 ^. qType
  , _addQuestionEventTitle = q2_aYes_fuQuestion1 ^. title
  , _addQuestionEventText = q2_aYes_fuQuestion1 ^. text
  , _addQuestionEventRequiredLevel = q2_aYes_fuQuestion1 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 :: AddQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "8ced5634-a879-4da2-b7c9-158ca6a4e0e3"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _addQuestionEventQuestionUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
  , _addQuestionEventQType = q2_aYes_fuq1_aYes_fuQuestion2 ^. qType
  , _addQuestionEventTitle = q2_aYes_fuq1_aYes_fuQuestion2 ^. title
  , _addQuestionEventText = q2_aYes_fuq1_aYes_fuQuestion2 ^. text
  , _addQuestionEventRequiredLevel = q2_aYes_fuq1_aYes_fuQuestion2 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 :: AddQuestionEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "6e9b591f-e6f9-46dd-85e8-a90fe4acc51c"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _addQuestionEventQuestionUuid = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. uuid
  , _addQuestionEventQType = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. qType
  , _addQuestionEventTitle = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. title
  , _addQuestionEventText = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. text
  , _addQuestionEventRequiredLevel = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain = Nothing
  }

a_km1_ch2_ansYes6_fuq4 :: AddQuestionEvent
a_km1_ch2_ansYes6_fuq4 =
  AddQuestionEvent
  { _addQuestionEventUuid = fromJust $ U.fromString "c626fd42-80b8-4fd2-a16b-d38eeb8262f1"
  , _addQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q4_ait1_q6_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _addQuestionEventQuestionUuid = q4_ait1_q6_aYes_followUpQuestion4 ^. uuid
  , _addQuestionEventQType = q4_ait1_q6_aYes_followUpQuestion4 ^. qType
  , _addQuestionEventTitle = q4_ait1_q6_aYes_followUpQuestion4 ^. title
  , _addQuestionEventText = q4_ait1_q6_aYes_followUpQuestion4 ^. text
  , _addQuestionEventRequiredLevel = q4_ait1_q6_aYes_followUpQuestion4 ^. requiredLevel
  , _addQuestionEventAnswerItemTemplatePlain =
      Just
        AnswerItemTemplatePlain
        {_answerItemTemplatePlainTitle = (fromJust $ q4_ait1_q6_aYes_followUpQuestion4 ^. answerItemTemplate) ^. title}
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: EditQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _editQuestionEventQuestionUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
  , _editQuestionEventQType = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Changed ^. qType
  , _editQuestionEventTitle = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Changed ^. title
  , _editQuestionEventText = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Changed ^. text
  , _editQuestionEventRequiredLevel = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Changed ^. requiredLevel
  , _editQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editQuestionEventAnswerIds = ChangedValue $ getAnwerIds q2_aYes_fuq1_aYes_fuQuestion2Changed
  , _editQuestionEventExpertIds = ChangedValue $ getExpertIds q2_aYes_fuq1_aYes_fuQuestion2Changed
  , _editQuestionEventReferenceIds = ChangedValue $ getReferenceIds q2_aYes_fuq1_aYes_fuQuestion2Changed
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 :: EditQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _editQuestionEventQuestionUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
  , _editQuestionEventQType = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Changed ^. qType
  , _editQuestionEventTitle = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Changed ^. title
  , _editQuestionEventText = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Changed ^. text
  , _editQuestionEventRequiredLevel = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Changed ^. requiredLevel
  , _editQuestionEventAnswerItemTemplatePlainWithIds = NothingChanged
  , _editQuestionEventAnswerIds =
      ChangedValue $ Just [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
  , _editQuestionEventExpertIds = ChangedValue $ getExpertIds q2_aYes_fuq1_aYes_fuQuestion2
  , _editQuestionEventReferenceIds = ChangedValue $ getReferenceIds q2_aYes_fuq1_aYes_fuQuestion2
  }

e_km1_ch2_ansMaybe6_fuq4 :: EditQuestionEvent
e_km1_ch2_ansMaybe6_fuq4 =
  EditQuestionEvent
  { _editQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_q6_answerNo ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _editQuestionEventQuestionUuid = q4_ait1_q6_aYes_followUpQuestion4Changed ^. uuid
  , _editQuestionEventQType = ChangedValue $ q4_ait1_q6_aYes_followUpQuestion4Changed ^. qType
  , _editQuestionEventTitle = ChangedValue $ q4_ait1_q6_aYes_followUpQuestion4Changed ^. title
  , _editQuestionEventText = ChangedValue $ q4_ait1_q6_aYes_followUpQuestion4Changed ^. text
  , _editQuestionEventRequiredLevel = ChangedValue $ q4_ait1_q6_aYes_followUpQuestion4Changed ^. requiredLevel
  , _editQuestionEventAnswerItemTemplatePlainWithIds =
      ChangedValue . Just $
      AnswerItemTemplatePlainWithIds
      { _answerItemTemplatePlainWithIdsTitle = "EDITED: fup 4 template title"
      , _answerItemTemplatePlainWithIdsQuestionIds =
          [q4_ait1_q6_aYes_fuq4_ait_question2 ^. uuid, q4_ait1_q6_aYes_fuq4_ait_question1 ^. uuid]
      }
  , _editQuestionEventAnswerIds = NothingChanged
  , _editQuestionEventExpertIds = NothingChanged
  , _editQuestionEventReferenceIds = NothingChanged
  }

d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: DeleteQuestionEvent
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "db69d694-cfb6-4461-8a13-81c01638f348"
  , _deleteQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _deleteQuestionEventQuestionUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_eAlbert :: AddExpertEvent
a_km1_ch1_q2_eAlbert =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "ec76054f-d059-4a5f-81c9-1817004a913c"
  , _addExpertEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addExpertEventExpertUuid = expertAlbert ^. uuid
  , _addExpertEventName = expertAlbert ^. name
  , _addExpertEventEmail = expertAlbert ^. email
  }

a_km1_ch2_q6_eAlbert :: AddExpertEvent
a_km1_ch2_q6_eAlbert =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "eb6bb073-ecba-4cd0-91a3-ff31d374601f"
  , _addExpertEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addExpertEventExpertUuid = expertAlbert ^. uuid
  , _addExpertEventName = expertAlbert ^. name
  , _addExpertEventEmail = expertAlbert ^. email
  }

a_km1_ch1_q2_eNikola :: AddExpertEvent
a_km1_ch1_q2_eNikola =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "40bb45bd-4195-4430-ac8f-16ac5a61ece0"
  , _addExpertEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addExpertEventExpertUuid = expertNikola ^. uuid
  , _addExpertEventName = expertNikola ^. name
  , _addExpertEventEmail = expertNikola ^. email
  }

a_km1_ch2_q6_eNikola :: AddExpertEvent
a_km1_ch2_q6_eNikola =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "53653d05-6d5a-4b76-bbc6-15ca8314ad69"
  , _addExpertEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addExpertEventExpertUuid = expertNikola ^. uuid
  , _addExpertEventName = expertNikola ^. name
  , _addExpertEventEmail = expertNikola ^. email
  }

a_km1_ch1_q2_eIsaac :: AddExpertEvent
a_km1_ch1_q2_eIsaac =
  AddExpertEvent
  { _addExpertEventUuid = fromJust $ U.fromString "2d5eedae-1782-44ac-9d4e-3db769161448"
  , _addExpertEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addExpertEventExpertUuid = expertIsaac ^. uuid
  , _addExpertEventName = expertIsaac ^. name
  , _addExpertEventEmail = expertIsaac ^. email
  }

e_km1_ch1_q2_eAlbert :: EditExpertEvent
e_km1_ch1_q2_eAlbert =
  EditExpertEvent
  { _editExpertEventUuid = fromJust $ U.fromString "01686131-2423-4d97-a949-4fea2c9ce3b7"
  , _editExpertEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editExpertEventExpertUuid = expertAlbert ^. uuid
  , _editExpertEventName = ChangedValue $ expertAlbertChanged ^. name
  , _editExpertEventEmail = ChangedValue $ expertAlbertChanged ^. email
  }

d_km1_ch1_q2_eNikola :: DeleteExpertEvent
d_km1_ch1_q2_eNikola =
  DeleteExpertEvent
  { _deleteExpertEventUuid = fromJust $ U.fromString "f20bc988-6d44-4051-990d-d16b24f369ac"
  , _deleteExpertEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _deleteExpertEventExpertUuid = expertNikola ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_rCh1 :: AddReferenceEvent
a_km1_ch1_q2_rCh1 =
  AddResourcePageReferenceEvent' $
  AddResourcePageReferenceEvent
  { _addResourcePageReferenceEventUuid = fromJust $ U.fromString "1177d72f-b7d8-466d-ad33-d5f82d0f192a"
  , _addResourcePageReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addResourcePageReferenceEventReferenceUuid = referenceCh1 ^. uuid
  , _addResourcePageReferenceEventShortUuid = referenceCh1 ^. shortUuid
  }

a_km1_ch2_q6_rCh1 :: AddReferenceEvent
a_km1_ch2_q6_rCh1 =
  AddResourcePageReferenceEvent' $
  AddResourcePageReferenceEvent
  { _addResourcePageReferenceEventUuid = fromJust $ U.fromString "a3f6ee9a-803f-4911-9566-734a6358913a"
  , _addResourcePageReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addResourcePageReferenceEventReferenceUuid = referenceCh1 ^. uuid
  , _addResourcePageReferenceEventShortUuid = referenceCh1 ^. shortUuid
  }

a_km1_ch1_q2_rCh2 :: AddReferenceEvent
a_km1_ch1_q2_rCh2 =
  AddURLReferenceEvent' $
  AddURLReferenceEvent
  { _addURLReferenceEventUuid = fromJust $ U.fromString "4814f50f-8838-4b53-8b18-c0f8c568220e"
  , _addURLReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addURLReferenceEventReferenceUuid = referenceCh2 ^. uuid
  , _addURLReferenceEventUrl = referenceCh2 ^. url
  , _addURLReferenceEventAnchor = referenceCh2 ^. anchor
  }

a_km1_ch2_q6_rCh2 :: AddReferenceEvent
a_km1_ch2_q6_rCh2 =
  AddURLReferenceEvent' $
  AddURLReferenceEvent
  { _addURLReferenceEventUuid = fromJust $ U.fromString "a4ae3400-dd3c-41ab-b796-4bf9d0bdafe7"
  , _addURLReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_ait1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addURLReferenceEventReferenceUuid = referenceCh2 ^. uuid
  , _addURLReferenceEventUrl = referenceCh2 ^. url
  , _addURLReferenceEventAnchor = referenceCh2 ^. anchor
  }

a_km1_ch1_q2_rCh3 :: AddReferenceEvent
a_km1_ch1_q2_rCh3 =
  AddCrossReferenceEvent' $
  AddCrossReferenceEvent
  { _addCrossReferenceEventUuid = fromJust $ U.fromString "45d8ec86-34bc-4e8f-b42a-48a567a77d8b"
  , _addCrossReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addCrossReferenceEventReferenceUuid = referenceCh3 ^. uuid
  , _addCrossReferenceEventTargetUuid = referenceCh3 ^. targetUuid
  , _addCrossReferenceEventDescription = referenceCh3 ^. description
  }

e_km1_ch1_q2_rCh1 :: EditReferenceEvent
e_km1_ch1_q2_rCh1 =
  EditResourcePageReferenceEvent' $
  EditResourcePageReferenceEvent
  { _editResourcePageReferenceEventUuid = fromJust $ U.fromString "08cd9afc-d416-48ab-8669-17e87ceb15dc"
  , _editResourcePageReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editResourcePageReferenceEventReferenceUuid = referenceCh1 ^. uuid
  , _editResourcePageReferenceEventShortUuid = ChangedValue $ referenceCh1Changed ^. shortUuid
  }

d_km1_ch1_q2_rCh2 :: DeleteReferenceEvent
d_km1_ch1_q2_rCh2 =
  DeleteURLReferenceEvent' $
  DeleteURLReferenceEvent
  { _deleteURLReferenceEventUuid = fromJust $ U.fromString "3cc15f31-4801-404f-ba48-6b91f77d1abe"
  , _deleteURLReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _deleteURLReferenceEventReferenceUuid = referenceCh2 ^. uuid
  }
