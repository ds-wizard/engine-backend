module Database.Migration.Development.Event.Data.Events where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.Experts
import Database.Migration.Development.KnowledgeModel.Data.Integrations
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.KnowledgeModel.Data.References
import Database.Migration.Development.KnowledgeModel.Data.Tags
import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.EventPath
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent
import Model.KnowledgeModel.KnowledgeModelLenses

a_km1 :: AddKnowledgeModelEvent
a_km1 =
  AddKnowledgeModelEvent
  { _addKnowledgeModelEventUuid = fromJust $ U.fromString "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f"
  , _addKnowledgeModelEventPath = []
  , _addKnowledgeModelEventKmUuid = km1WithoutChaptersAndTagsAndIntegrations ^. uuid
  , _addKnowledgeModelEventName = km1WithoutChaptersAndTagsAndIntegrations ^. name
  }

e_km1 :: EditKnowledgeModelEvent
e_km1 =
  EditKnowledgeModelEvent
  { _editKnowledgeModelEventUuid = fromJust $ U.fromString "8294a55d-642d-416c-879b-5a42a4430c24"
  , _editKnowledgeModelEventPath = []
  , _editKnowledgeModelEventKmUuid = km1 ^. uuid
  , _editKnowledgeModelEventName = ChangedValue $ km1Edited ^. name
  , _editKnowledgeModelEventChapterUuids = ChangedValue $ km1Edited ^.. chapters . traverse . uuid
  , _editKnowledgeModelEventTagUuids = ChangedValue $ km1Edited ^.. tags . traverse . uuid
  , _editKnowledgeModelEventIntegrationUuids = ChangedValue $ km1Edited ^.. integrations . traverse . uuid
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
  { _addChapterEventUuid = fromJust $ U.fromString "6eaa2b47-711d-4187-98f8-fccdce94db9b"
  , _addChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _addChapterEventChapterUuid = chapter3 ^. uuid
  , _addChapterEventTitle = chapter3 ^. title
  , _addChapterEventText = chapter3 ^. text
  }

a_km1_ch4 :: AddChapterEvent
a_km1_ch4 =
  AddChapterEvent
  { _addChapterEventUuid = fromJust $ U.fromString "6585a64d-c75b-47fc-a86e-e0c8e773528f"
  , _addChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _addChapterEventChapterUuid = chapter4WithoutQuestions ^. uuid
  , _addChapterEventTitle = chapter4WithoutQuestions ^. title
  , _addChapterEventText = chapter4WithoutQuestions ^. text
  }

e_km1_ch1 :: EditChapterEvent
e_km1_ch1 =
  EditChapterEvent
  { _editChapterEventUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _editChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _editChapterEventChapterUuid = chapter1 ^. uuid
  , _editChapterEventTitle = ChangedValue $ chapter1Edited ^. title
  , _editChapterEventText = ChangedValue $ chapter1Edited ^. text
  , _editChapterEventQuestionUuids = ChangedValue $ getQuestionUuid <$> chapter1Edited ^. questions
  }

e_km1_ch1_2 :: EditChapterEvent
e_km1_ch1_2 =
  EditChapterEvent
  { _editChapterEventUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _editChapterEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _editChapterEventChapterUuid = chapter1 ^. uuid
  , _editChapterEventTitle = ChangedValue $ "TWICE: " ++ chapter1Edited ^. title
  , _editChapterEventText = ChangedValue $ chapter1Edited ^. text
  , _editChapterEventQuestionUuids = ChangedValue $ getQuestionUuid <$> chapter1Edited ^. questions
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
a_km1_ch1_q1' :: AddQuestionEvent
a_km1_ch1_q1' = AddValueQuestionEvent' a_km1_ch1_q1

a_km1_ch1_q1 :: AddValueQuestionEvent
a_km1_ch1_q1 =
  AddValueQuestionEvent
  { _addValueQuestionEventUuid = fromJust $ U.fromString "71ae2ce9-553b-4ca2-a542-1bce04406c51"
  , _addValueQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addValueQuestionEventQuestionUuid = question1 ^. uuid
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
  , _addOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addOptionsQuestionEventQuestionUuid = question2 ^. uuid
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
  , _addOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addOptionsQuestionEventQuestionUuid = question3 ^. uuid
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
  , _addOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addOptionsQuestionEventQuestionUuid = question3 ^. uuid
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
  , _addListQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addListQuestionEventQuestionUuid = question4 ^. uuid
  , _addListQuestionEventTitle = question4 ^. title
  , _addListQuestionEventText = question4 ^. text
  , _addListQuestionEventRequiredLevel = question4 ^. requiredLevel
  , _addListQuestionEventTagUuids = question4 ^. tagUuids
  , _addListQuestionEventItemTemplateTitle = question4 ^. itemTemplateTitle
  }

a_km1_ch3_q9' :: AddQuestionEvent
a_km1_ch3_q9' = AddIntegrationQuestionEvent' a_km1_ch3_q9

a_km1_ch3_q9 :: AddIntegrationQuestionEvent
a_km1_ch3_q9 =
  AddIntegrationQuestionEvent
  { _addIntegrationQuestionEventUuid = fromJust $ U.fromString "51526318-2727-4113-993d-bae5d4abafcd"
  , _addIntegrationQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter3 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addIntegrationQuestionEventQuestionUuid = question9 ^. uuid
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
  , _addIntegrationQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter3 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _addIntegrationQuestionEventQuestionUuid = question10 ^. uuid
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
  , _editValueQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editValueQuestionEventQuestionUuid = question1Edited ^. uuid
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
  , _editOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editOptionsQuestionEventQuestionUuid = question1WithNewType ^. uuid
  , _editOptionsQuestionEventTitle = ChangedValue $ question1WithNewType ^. title
  , _editOptionsQuestionEventText = NothingChanged
  , _editOptionsQuestionEventRequiredLevel = NothingChanged
  , _editOptionsQuestionEventTagUuids = NothingChanged
  , _editOptionsQuestionEventExpertUuids = NothingChanged
  , _editOptionsQuestionEventReferenceUuids = NothingChanged
  , _editOptionsQuestionEventAnswerUuids = ChangedValue $ getAnwerUuids question1WithNewType
  }

e_km1_ch1_q2' :: EditQuestionEvent
e_km1_ch1_q2' = EditOptionsQuestionEvent' e_km1_ch1_q2

e_km1_ch1_q2 :: EditOptionsQuestionEvent
e_km1_ch1_q2 =
  EditOptionsQuestionEvent
  { _editOptionsQuestionEventUuid = fromJust $ U.fromString "1a01665b-e896-450d-b606-afc1dcca586b"
  , _editOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editOptionsQuestionEventQuestionUuid = question2 ^. uuid
  , _editOptionsQuestionEventTitle = ChangedValue $ question2Edited ^. title
  , _editOptionsQuestionEventText = ChangedValue $ question2Edited ^. text
  , _editOptionsQuestionEventRequiredLevel = ChangedValue $ question2Edited ^. requiredLevel
  , _editOptionsQuestionEventTagUuids = ChangedValue $ question2Edited ^. tagUuids
  , _editOptionsQuestionEventExpertUuids = ChangedValue $ getExpertUuids question2Edited'
  , _editOptionsQuestionEventReferenceUuids = ChangedValue $ getReferenceUuids question2Edited'
  , _editOptionsQuestionEventAnswerUuids = ChangedValue $ getAnwerUuids question2Edited
  }

e_km1_ch1_q2_second_edit' :: EditQuestionEvent
e_km1_ch1_q2_second_edit' = EditOptionsQuestionEvent' e_km1_ch1_q2_second_edit

e_km1_ch1_q2_second_edit :: EditOptionsQuestionEvent
e_km1_ch1_q2_second_edit =
  EditOptionsQuestionEvent
  { _editOptionsQuestionEventUuid = fromJust $ U.fromString "bf888b95-921d-4caa-88af-3309393d44c3"
  , _editOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editOptionsQuestionEventQuestionUuid = question2 ^. uuid
  , _editOptionsQuestionEventTitle = ChangedValue "New title"
  , _editOptionsQuestionEventText = ChangedValue $ question2Edited ^. text
  , _editOptionsQuestionEventRequiredLevel = ChangedValue $ question2Edited ^. requiredLevel
  , _editOptionsQuestionEventTagUuids = ChangedValue $ question2Edited ^. tagUuids
  , _editOptionsQuestionEventExpertUuids = ChangedValue $ getExpertUuids question2Edited'
  , _editOptionsQuestionEventReferenceUuids = ChangedValue $ getReferenceUuids question2Edited'
  , _editOptionsQuestionEventAnswerUuids = ChangedValue $ getAnwerUuids question2Edited
  }

e_km1_ch1_q2_type' :: EditQuestionEvent
e_km1_ch1_q2_type' = EditListQuestionEvent' e_km1_ch1_q2_type

e_km1_ch1_q2_type :: EditListQuestionEvent
e_km1_ch1_q2_type =
  EditListQuestionEvent
  { _editListQuestionEventUuid = fromJust $ U.fromString "2727c225-78e5-4d5f-a093-cfaadb6ea663"
  , _editListQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editListQuestionEventQuestionUuid = question2WithNewType ^. uuid
  , _editListQuestionEventTitle = ChangedValue $ question2WithNewType ^. title
  , _editListQuestionEventText = NothingChanged
  , _editListQuestionEventRequiredLevel = NothingChanged
  , _editListQuestionEventTagUuids = NothingChanged
  , _editListQuestionEventExpertUuids = NothingChanged
  , _editListQuestionEventReferenceUuids = NothingChanged
  , _editListQuestionEventItemTemplateTitle = ChangedValue $ question2WithNewType ^. itemTemplateTitle
  , _editListQuestionEventItemTemplateQuestionUuids =
      ChangedValue $ getQuestionUuid <$> (question4Edited ^. itemTemplateQuestions)
  }

e_km1_ch2_q4' :: EditQuestionEvent
e_km1_ch2_q4' = EditListQuestionEvent' e_km1_ch2_q4

e_km1_ch2_q4 :: EditListQuestionEvent
e_km1_ch2_q4 =
  EditListQuestionEvent
  { _editListQuestionEventUuid = fromJust $ U.fromString "7014c6de-a1c0-4c09-881a-c83c68a29de1"
  , _editListQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editListQuestionEventQuestionUuid = question4Edited ^. uuid
  , _editListQuestionEventTitle = ChangedValue $ question4Edited ^. title
  , _editListQuestionEventText = ChangedValue $ question4Edited ^. text
  , _editListQuestionEventRequiredLevel = ChangedValue $ question4Edited ^. requiredLevel
  , _editListQuestionEventTagUuids = ChangedValue $ question4Edited ^. tagUuids
  , _editListQuestionEventExpertUuids = ChangedValue $ getExpertUuids question4Edited'
  , _editListQuestionEventReferenceUuids = ChangedValue $ getReferenceUuids question4Edited'
  , _editListQuestionEventItemTemplateTitle = ChangedValue $ question4Edited ^. itemTemplateTitle
  , _editListQuestionEventItemTemplateQuestionUuids =
      ChangedValue $ getQuestionUuid <$> (question4Edited ^. itemTemplateQuestions)
  }

e_km1_ch2_q4_type' :: EditQuestionEvent
e_km1_ch2_q4_type' = EditIntegrationQuestionEvent' e_km1_ch2_q4_type

e_km1_ch2_q4_type :: EditIntegrationQuestionEvent
e_km1_ch2_q4_type =
  EditIntegrationQuestionEvent
  { _editIntegrationQuestionEventUuid = fromJust $ U.fromString "0f6f536c-aa1c-4d47-8cd7-46d611b43a56"
  , _editIntegrationQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editIntegrationQuestionEventQuestionUuid = question4WithNewType ^. uuid
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
  , _editIntegrationQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter3 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editIntegrationQuestionEventQuestionUuid = question9Edited ^. uuid
  , _editIntegrationQuestionEventTitle = ChangedValue $ question9Edited ^. title
  , _editIntegrationQuestionEventText = ChangedValue $ question9Edited ^. text
  , _editIntegrationQuestionEventRequiredLevel = ChangedValue $ question9Edited ^. requiredLevel
  , _editIntegrationQuestionEventTagUuids = ChangedValue $ question9Edited ^. tagUuids
  , _editIntegrationQuestionEventExpertUuids = ChangedValue $ getExpertUuids question9Edited'
  , _editIntegrationQuestionEventReferenceUuids = ChangedValue $ getReferenceUuids question9Edited'
  , _editIntegrationQuestionEventIntegrationUuid = ChangedValue $ question9Edited ^. integrationUuid
  , _editIntegrationQuestionEventProps = ChangedValue $ question9Edited ^. props
  }

e_km1_ch3_q9_type' :: EditQuestionEvent
e_km1_ch3_q9_type' = EditValueQuestionEvent' e_km1_ch3_q9_type

e_km1_ch3_q9_type :: EditValueQuestionEvent
e_km1_ch3_q9_type =
  EditValueQuestionEvent
  { _editValueQuestionEventUuid = fromJust $ U.fromString "91514dc3-29b1-469a-b0d9-5fc211df1c47"
  , _editValueQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter3 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      ]
  , _editValueQuestionEventQuestionUuid = question9WithNewType ^. uuid
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

a_km1_ch2_q4_it_q6_aNo :: AddAnswerEvent
a_km1_ch2_q4_it_q6_aNo =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "c0a67ce5-21b3-47c7-8624-c2da26fb494f"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q4_it1_q6_answerNo ^. uuid
  , _addAnswerEventLabel = q4_it1_q6_answerNo ^. label
  , _addAnswerEventAdvice = q4_it1_q6_answerNo ^. advice
  , _addAnswerEventMetricMeasures = q4_it1_q6_answerNo ^. metricMeasures
  }

a_km1_ch2_q4_it_q6_aYes :: AddAnswerEvent
a_km1_ch2_q4_it_q6_aYes =
  AddAnswerEvent
  { _addAnswerEventUuid = fromJust $ U.fromString "c5c42f99-613b-4b6c-ae5e-af784f51c483"
  , _addAnswerEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addAnswerEventAnswerUuid = q4_it1_q6_answerYes ^. uuid
  , _addAnswerEventLabel = q4_it1_q6_answerYes ^. label
  , _addAnswerEventAdvice = q4_it1_q6_answerYes ^. advice
  , _addAnswerEventMetricMeasures = q4_it1_q6_answerYes ^. metricMeasures
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
  , _editAnswerEventLabel = ChangedValue $ q2_answerYesEdited ^. label
  , _editAnswerEventAdvice = ChangedValue $ q2_answerYesEdited ^. advice
  , _editAnswerEventFollowUpUuids = ChangedValue $ getQuestionUuid <$> (q2_answerYesEdited ^. followUps)
  , _editAnswerEventMetricMeasures = ChangedValue $ q2_answerYesEdited ^. metricMeasures
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
  , _editAnswerEventLabel = ChangedValue $ q2_answerYesEdited ^. label
  , _editAnswerEventAdvice = ChangedValue $ q2_answerYesEdited ^. advice
  , _editAnswerEventFollowUpUuids = ChangedValue $ getQuestionUuid <$> (q2_answerYes ^. followUps)
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
a_km1_ch2_q4_it1_q5' :: AddQuestionEvent
a_km1_ch2_q4_it1_q5' = AddListQuestionEvent' a_km1_ch2_q4_it1_q5

a_km1_ch2_q4_it1_q5 :: AddListQuestionEvent
a_km1_ch2_q4_it1_q5 =
  AddListQuestionEvent
  { _addListQuestionEventUuid = fromJust $ U.fromString "5619d036-0130-47fa-9553-b73094eecd7e"
  , _addListQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addListQuestionEventQuestionUuid = q4_it1_question5 ^. uuid
  , _addListQuestionEventTitle = q4_it1_question5 ^. title
  , _addListQuestionEventText = q4_it1_question5 ^. text
  , _addListQuestionEventRequiredLevel = q4_it1_question5 ^. requiredLevel
  , _addListQuestionEventTagUuids = q4_it1_question5 ^. tagUuids
  , _addListQuestionEventItemTemplateTitle = q4_it1_question5 ^. itemTemplateTitle
  }

a_km1_ch2_q4_it1_q6' :: AddQuestionEvent
a_km1_ch2_q4_it1_q6' = AddOptionsQuestionEvent' a_km1_ch2_q4_it1_q6

a_km1_ch2_q4_it1_q6 :: AddOptionsQuestionEvent
a_km1_ch2_q4_it1_q6 =
  AddOptionsQuestionEvent
  { _addOptionsQuestionEventUuid = fromJust $ U.fromString "5ac56741-b93a-42f5-9beb-f22100e4342d"
  , _addOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addOptionsQuestionEventQuestionUuid = q4_it1_question6 ^. uuid
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
  , _addOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_q6_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        { _eventPathItemUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
        , _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION
        }
      ]
  , _addOptionsQuestionEventQuestionUuid = q4_it1_q6_aYes_fuq4_it_question1 ^. uuid
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
  , _addOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_q6_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        { _eventPathItemUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
        , _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION
        }
      ]
  , _addOptionsQuestionEventQuestionUuid = q4_it1_q6_aYes_fuq4_it_question2 ^. uuid
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
  , _addValueQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question5 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addValueQuestionEventQuestionUuid = q4_it1_q5_it2_question7 ^. uuid
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
  , _addValueQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question5 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addValueQuestionEventQuestionUuid = q4_it1_q5_it2_question8 ^. uuid
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
  , _editListQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editListQuestionEventQuestionUuid = q4_it1_question5Edited ^. uuid
  , _editListQuestionEventTitle = ChangedValue $ q4_it1_question5Edited ^. title
  , _editListQuestionEventText = ChangedValue $ q4_it1_question5Edited ^. text
  , _editListQuestionEventRequiredLevel = ChangedValue $ q4_it1_question5Edited ^. requiredLevel
  , _editListQuestionEventTagUuids = ChangedValue $ q4_it1_question5Edited ^. tagUuids
  , _editListQuestionEventExpertUuids = NothingChanged
  , _editListQuestionEventReferenceUuids = NothingChanged
  , _editListQuestionEventItemTemplateTitle = ChangedValue $ "EDITED: Template Title 2"
  , _editListQuestionEventItemTemplateQuestionUuids =
      ChangedValue $ [q4_it1_q5_it2_question8 ^. uuid, q4_it1_q5_it2_question7 ^. uuid]
  }

e_km1_ch2_q4_it1_q6' :: EditQuestionEvent
e_km1_ch2_q4_it1_q6' = EditOptionsQuestionEvent' e_km1_ch2_q4_it1_q6

e_km1_ch2_q4_it1_q6 :: EditOptionsQuestionEvent
e_km1_ch2_q4_it1_q6 =
  EditOptionsQuestionEvent
  { _editOptionsQuestionEventUuid = fromJust $ U.fromString "f5c5ccfd-619b-4110-807a-39ede6d31cae"
  , _editOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editOptionsQuestionEventQuestionUuid = q4_it1_question6Edited ^. uuid
  , _editOptionsQuestionEventTitle = ChangedValue $ q4_it1_question6Edited ^. title
  , _editOptionsQuestionEventText = ChangedValue $ q4_it1_question6Edited ^. text
  , _editOptionsQuestionEventRequiredLevel = ChangedValue $ q4_it1_question6Edited ^. requiredLevel
  , _editOptionsQuestionEventTagUuids = ChangedValue $ q4_it1_question6Edited ^. tagUuids
  , _editOptionsQuestionEventExpertUuids = ChangedValue $ getExpertUuids q4_it1_question6Edited'
  , _editOptionsQuestionEventReferenceUuids = ChangedValue $ getReferenceUuids q4_it1_question6Edited'
  , _editOptionsQuestionEventAnswerUuids = ChangedValue $ getAnwerUuids q4_it1_question6Edited
  }

d_km1_ch2_q4_it1_q5 :: DeleteQuestionEvent
d_km1_ch2_q4_it1_q5 =
  DeleteQuestionEvent
  { _deleteQuestionEventUuid = fromJust $ U.fromString "424d19cb-a79f-4da0-b7f6-33363c32b7fd"
  , _deleteQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _deleteQuestionEventQuestionUuid = q4_it1_question5 ^. uuid
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
  , _addOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _addOptionsQuestionEventQuestionUuid = q2_aYes_fuQuestion1 ^. uuid
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
  , _addOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _addOptionsQuestionEventQuestionUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
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
  , _addOptionsQuestionEventPath =
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
  , _addOptionsQuestionEventQuestionUuid = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 ^. uuid
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
  , _addListQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_q6_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _addListQuestionEventQuestionUuid = q4_it1_q6_aYes_followUpQuestion4 ^. uuid
  , _addListQuestionEventTitle = q4_it1_q6_aYes_followUpQuestion4 ^. title
  , _addListQuestionEventText = q4_it1_q6_aYes_followUpQuestion4 ^. text
  , _addListQuestionEventRequiredLevel = q4_it1_q6_aYes_followUpQuestion4 ^. requiredLevel
  , _addListQuestionEventTagUuids = q4_it1_q6_aYes_followUpQuestion4 ^. tagUuids
  , _addListQuestionEventItemTemplateTitle = q4_it1_q6_aYes_followUpQuestion4 ^. itemTemplateTitle
  }

a_km1_ch2_ansYes6_fuq5' :: AddQuestionEvent
a_km1_ch2_ansYes6_fuq5' = AddIntegrationQuestionEvent' a_km1_ch2_ansYes6_fuq5

a_km1_ch2_ansYes6_fuq5 :: AddIntegrationQuestionEvent
a_km1_ch2_ansYes6_fuq5 =
  AddIntegrationQuestionEvent
  { _addIntegrationQuestionEventUuid = fromJust $ U.fromString "11872ad2-0d3d-4ab6-b81c-17d234bab6ba"
  , _addIntegrationQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_q6_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _addIntegrationQuestionEventQuestionUuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
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
  , _editOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _editOptionsQuestionEventQuestionUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
  , _editOptionsQuestionEventTitle = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. title
  , _editOptionsQuestionEventText = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. text
  , _editOptionsQuestionEventRequiredLevel = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. requiredLevel
  , _editOptionsQuestionEventTagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. tagUuids
  , _editOptionsQuestionEventExpertUuids = ChangedValue $ getExpertUuids q2_aYes_fuq1_aYes_fuQuestion2Edited'
  , _editOptionsQuestionEventReferenceUuids = ChangedValue $ getReferenceUuids q2_aYes_fuq1_aYes_fuQuestion2Edited'
  , _editOptionsQuestionEventAnswerUuids = ChangedValue $ getAnwerUuids q2_aYes_fuq1_aYes_fuQuestion2Edited
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2' :: EditQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2' = EditOptionsQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 :: EditOptionsQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 =
  EditOptionsQuestionEvent
  { _editOptionsQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editOptionsQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q2_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuQuestion1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem
        {_eventPathItemUuid = q2_aYes_fuq1_answerYes ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _editOptionsQuestionEventQuestionUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
  , _editOptionsQuestionEventTitle = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. title
  , _editOptionsQuestionEventText = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. text
  , _editOptionsQuestionEventRequiredLevel = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. requiredLevel
  , _editOptionsQuestionEventTagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited ^. tagUuids
  , _editOptionsQuestionEventExpertUuids = ChangedValue $ getExpertUuids q2_aYes_fuq1_aYes_fuQuestion2'
  , _editOptionsQuestionEventReferenceUuids = ChangedValue $ getReferenceUuids q2_aYes_fuq1_aYes_fuQuestion2'
  , _editOptionsQuestionEventAnswerUuids =
      ChangedValue $ [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
  }

e_km1_ch2_ansMaybe6_fuq4' :: EditQuestionEvent
e_km1_ch2_ansMaybe6_fuq4' = EditListQuestionEvent' e_km1_ch2_ansMaybe6_fuq4

e_km1_ch2_ansMaybe6_fuq4 :: EditListQuestionEvent
e_km1_ch2_ansMaybe6_fuq4 =
  EditListQuestionEvent
  { _editListQuestionEventUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _editListQuestionEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_q6_answerNo ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__ANSWER}
      ]
  , _editListQuestionEventQuestionUuid = q4_it1_q6_aYes_followUpQuestion4Edited ^. uuid
  , _editListQuestionEventTitle = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. title
  , _editListQuestionEventText = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. text
  , _editListQuestionEventRequiredLevel = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. requiredLevel
  , _editListQuestionEventTagUuids = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited ^. tagUuids
  , _editListQuestionEventExpertUuids = NothingChanged
  , _editListQuestionEventReferenceUuids = NothingChanged
  , _editListQuestionEventItemTemplateTitle = ChangedValue $ "EDITED: fup 4 template title"
  , _editListQuestionEventItemTemplateQuestionUuids =
      ChangedValue $ [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid, q4_it1_q6_aYes_fuq4_it_question1 ^. uuid]
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
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
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
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
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
  , _editExpertEventName = ChangedValue $ expertAlbertEdited ^. name
  , _editExpertEventEmail = ChangedValue $ expertAlbertEdited ^. email
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
a_km1_ch1_q2_rCh1' :: AddReferenceEvent
a_km1_ch1_q2_rCh1' = AddResourcePageReferenceEvent' a_km1_ch1_q2_rCh1

a_km1_ch1_q2_rCh1 :: AddResourcePageReferenceEvent
a_km1_ch1_q2_rCh1 =
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

a_km1_ch2_q6_rCh1' :: AddReferenceEvent
a_km1_ch2_q6_rCh1' = AddResourcePageReferenceEvent' a_km1_ch2_q6_rCh1

a_km1_ch2_q6_rCh1 :: AddResourcePageReferenceEvent
a_km1_ch2_q6_rCh1 =
  AddResourcePageReferenceEvent
  { _addResourcePageReferenceEventUuid = fromJust $ U.fromString "a3f6ee9a-803f-4911-9566-734a6358913a"
  , _addResourcePageReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addResourcePageReferenceEventReferenceUuid = referenceCh1 ^. uuid
  , _addResourcePageReferenceEventShortUuid = referenceCh1 ^. shortUuid
  }

a_km1_ch1_q2_rCh2' :: AddReferenceEvent
a_km1_ch1_q2_rCh2' = AddURLReferenceEvent' a_km1_ch1_q2_rCh2

a_km1_ch1_q2_rCh2 :: AddURLReferenceEvent
a_km1_ch1_q2_rCh2 =
  AddURLReferenceEvent
  { _addURLReferenceEventUuid = fromJust $ U.fromString "4814f50f-8838-4b53-8b18-c0f8c568220e"
  , _addURLReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addURLReferenceEventReferenceUuid = referenceCh2 ^. uuid
  , _addURLReferenceEventUrl = referenceCh2 ^. url
  , _addURLReferenceEventLabel = referenceCh2 ^. label
  }

a_km1_ch2_q6_rCh2' :: AddReferenceEvent
a_km1_ch2_q6_rCh2' = AddURLReferenceEvent' a_km1_ch2_q6_rCh2

a_km1_ch2_q6_rCh2 :: AddURLReferenceEvent
a_km1_ch2_q6_rCh2 =
  AddURLReferenceEvent
  { _addURLReferenceEventUuid = fromJust $ U.fromString "a4ae3400-dd3c-41ab-b796-4bf9d0bdafe7"
  , _addURLReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question4 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      , EventPathItem {_eventPathItemUuid = q4_it1_question6 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _addURLReferenceEventReferenceUuid = referenceCh2 ^. uuid
  , _addURLReferenceEventUrl = referenceCh2 ^. url
  , _addURLReferenceEventLabel = referenceCh2 ^. label
  }

a_km1_ch1_q2_rCh3' :: AddReferenceEvent
a_km1_ch1_q2_rCh3' = AddCrossReferenceEvent' a_km1_ch1_q2_rCh3

a_km1_ch1_q2_rCh3 :: AddCrossReferenceEvent
a_km1_ch1_q2_rCh3 =
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

e_km1_ch1_q2_rCh1' :: EditReferenceEvent
e_km1_ch1_q2_rCh1' = EditResourcePageReferenceEvent' e_km1_ch1_q2_rCh1

e_km1_ch1_q2_rCh1 :: EditResourcePageReferenceEvent
e_km1_ch1_q2_rCh1 =
  EditResourcePageReferenceEvent
  { _editResourcePageReferenceEventUuid = fromJust $ U.fromString "08cd9afc-d416-48ab-8669-17e87ceb15dc"
  , _editResourcePageReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editResourcePageReferenceEventReferenceUuid = referenceCh1 ^. uuid
  , _editResourcePageReferenceEventShortUuid = ChangedValue $ referenceCh1Edited ^. shortUuid
  }

e_km1_ch1_q2_rCh1_type' :: EditReferenceEvent
e_km1_ch1_q2_rCh1_type' = EditURLReferenceEvent' e_km1_ch1_q2_rCh1_type

e_km1_ch1_q2_rCh1_type :: EditURLReferenceEvent
e_km1_ch1_q2_rCh1_type =
  EditURLReferenceEvent
  { _editURLReferenceEventUuid = fromJust $ U.fromString "4e1058cf-9044-42a0-901c-816bd6847b17"
  , _editURLReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editURLReferenceEventReferenceUuid = referenceCh1WithNewType ^. uuid
  , _editURLReferenceEventUrl = ChangedValue $ referenceCh1WithNewType ^. url
  , _editURLReferenceEventLabel = ChangedValue $ referenceCh1WithNewType ^. label
  }

e_km1_ch1_q2_rCh2' :: EditReferenceEvent
e_km1_ch1_q2_rCh2' = EditURLReferenceEvent' e_km1_ch1_q2_rCh2

e_km1_ch1_q2_rCh2 :: EditURLReferenceEvent
e_km1_ch1_q2_rCh2 =
  EditURLReferenceEvent
  { _editURLReferenceEventUuid = fromJust $ U.fromString "f96588ae-1657-406e-9810-1d00f5e24a96"
  , _editURLReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editURLReferenceEventReferenceUuid = referenceCh2Edited ^. uuid
  , _editURLReferenceEventUrl = ChangedValue $ referenceCh2Edited ^. url
  , _editURLReferenceEventLabel = ChangedValue $ referenceCh2Edited ^. label
  }

e_km1_ch1_q2_rCh2_type' :: EditReferenceEvent
e_km1_ch1_q2_rCh2_type' = EditCrossReferenceEvent' e_km1_ch1_q2_rCh2_type

e_km1_ch1_q2_rCh2_type :: EditCrossReferenceEvent
e_km1_ch1_q2_rCh2_type =
  EditCrossReferenceEvent
  { _editCrossReferenceEventUuid = fromJust $ U.fromString "e0a19e9d-fb36-47b3-bc23-f752f7403937"
  , _editCrossReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editCrossReferenceEventReferenceUuid = referenceCh2WithNewType ^. uuid
  , _editCrossReferenceEventTargetUuid = ChangedValue $ referenceCh2WithNewType ^. targetUuid
  , _editCrossReferenceEventDescription = ChangedValue $ referenceCh2WithNewType ^. description
  }

e_km1_ch1_q2_rCh3' :: EditReferenceEvent
e_km1_ch1_q2_rCh3' = EditCrossReferenceEvent' e_km1_ch1_q2_rCh3

e_km1_ch1_q2_rCh3 :: EditCrossReferenceEvent
e_km1_ch1_q2_rCh3 =
  EditCrossReferenceEvent
  { _editCrossReferenceEventUuid = fromJust $ U.fromString "d3a7b6a6-9e87-4308-a103-88245537c26e"
  , _editCrossReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editCrossReferenceEventReferenceUuid = referenceCh3Edited ^. uuid
  , _editCrossReferenceEventTargetUuid = ChangedValue $ referenceCh3Edited ^. targetUuid
  , _editCrossReferenceEventDescription = ChangedValue $ referenceCh3Edited ^. description
  }

e_km1_ch1_q2_rCh3_type' :: EditReferenceEvent
e_km1_ch1_q2_rCh3_type' = EditResourcePageReferenceEvent' e_km1_ch1_q2_rCh3_type

e_km1_ch1_q2_rCh3_type :: EditResourcePageReferenceEvent
e_km1_ch1_q2_rCh3_type =
  EditResourcePageReferenceEvent
  { _editResourcePageReferenceEventUuid = fromJust $ U.fromString "f8528e3b-4904-4ad8-87b8-809d7e40c087"
  , _editResourcePageReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _editResourcePageReferenceEventReferenceUuid = referenceCh3WithNewType ^. uuid
  , _editResourcePageReferenceEventShortUuid = ChangedValue $ referenceCh3WithNewType ^. shortUuid
  }

d_km1_ch1_q2_rCh2 :: DeleteReferenceEvent
d_km1_ch1_q2_rCh2 =
  DeleteReferenceEvent
  { _deleteReferenceEventUuid = fromJust $ U.fromString "3cc15f31-4801-404f-ba48-6b91f77d1abe"
  , _deleteReferenceEventPath =
      [ EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}
      , EventPathItem {_eventPathItemUuid = chapter1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__CHAPTER}
      , EventPathItem {_eventPathItemUuid = question2 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__QUESTION}
      ]
  , _deleteReferenceEventReferenceUuid = referenceCh2 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_tds :: AddTagEvent
a_km1_tds =
  AddTagEvent
  { _addTagEventUuid = fromJust $ U.fromString "dedc4a9d-00d9-41b6-8494-a10a238be03b"
  , _addTagEventPath = [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _addTagEventTagUuid = tagDataScience ^. uuid
  , _addTagEventName = tagDataScience ^. name
  , _addTagEventDescription = tagDataScience ^. description
  , _addTagEventColor = tagDataScience ^. color
  }

a_km1_tbi :: AddTagEvent
a_km1_tbi =
  AddTagEvent
  { _addTagEventUuid = fromJust $ U.fromString "b6b0e53c-5702-403c-950c-e04960e09e73"
  , _addTagEventPath = [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _addTagEventTagUuid = tagBioInformatic ^. uuid
  , _addTagEventName = tagBioInformatic ^. name
  , _addTagEventDescription = tagBioInformatic ^. description
  , _addTagEventColor = tagBioInformatic ^. color
  }

e_km1_tds :: EditTagEvent
e_km1_tds =
  EditTagEvent
  { _editTagEventUuid = fromJust $ U.fromString "f68f764b-48d1-4b30-8d53-48cfa2752801"
  , _editTagEventPath = [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _editTagEventTagUuid = tagDataScienceEdited ^. uuid
  , _editTagEventName = ChangedValue $ tagDataScienceEdited ^. name
  , _editTagEventDescription = ChangedValue $ tagDataScienceEdited ^. description
  , _editTagEventColor = ChangedValue $ tagDataScienceEdited ^. color
  }

d_km1_tds :: DeleteTagEvent
d_km1_tds =
  DeleteTagEvent
  { _deleteTagEventUuid = fromJust $ U.fromString "969d00c2-062d-4763-a372-536d486c532f"
  , _deleteTagEventPath = [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _deleteTagEventTagUuid = tagDataScience ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_iop :: AddIntegrationEvent
a_km1_iop =
  AddIntegrationEvent
  { _addIntegrationEventUuid = fromJust $ U.fromString "3f94cb01-6f92-4eb6-975b-385c02b831bc"
  , _addIntegrationEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _addIntegrationEventIntegrationUuid = ontologyPortal ^. uuid
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
  , _addIntegrationEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _addIntegrationEventIntegrationUuid = bioPortal ^. uuid
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
  , _editIntegrationEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _editIntegrationEventIntegrationUuid = ontologyPortalEdited ^. uuid
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
  , _deleteIntegrationEventPath =
      [EventPathItem {_eventPathItemUuid = km1 ^. uuid, _eventPathItemPType = _EVENT_PATH_ITEM__KM}]
  , _deleteIntegrationEventIntegrationUuid = ontologyPortal ^. uuid
  }
