module Wizard.Specs.Service.KnowledgeModel.Squash.SquasherSpec where

import Test.Hspec

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Reference.ReferenceEvent
import Wizard.Service.KnowledgeModel.Squash.Squasher

-- ---------------------------
-- TESTS
-- ---------------------------
squasherSpec =
  describe "Squasher" $ do
    it "squash" $
      -- GIVEN: prepare data
      do
        let sourceEvents =
              [ a_q1
              , a_ch1
              , e_ch1_label_1
              , e_q1_title_1
              , a_ref1
              , a_q2
              , e_q2_title
              , e_q1_text
              , a_q3
              , e_q1_answerUuids
              , e_q1_answerUuids_title
              , e_q1_title_2
              , e_q1_type
              , e_ref1_type
              , e_ref1_url
              , e_ch1_label_2
              , e_ch1_label_3_day2
              ]
        -- AND: prepare expectation
        let expEvents =
              [ a_q1
              , a_ch1
              , e_ch1_label_2 {createdAt = e_ch1_label_1.createdAt}
              , e_q1_text {content = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_text__content {title = e_q1_title_1__content.title}, createdAt = e_q1_title_1.createdAt}
              , a_ref1
              , a_q2
              , e_q2_title
              , a_q3
              , e_q1_title_2 {content = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_title_2__content {answerUuids = e_q1_answerUuids_title__content.answerUuids}, createdAt = e_q1_answerUuids.createdAt}
              , e_q1_type
              , e_ref1_url {createdAt = e_ref1_type.createdAt}
              , e_ch1_label_3_day2
              ]
        -- WHEN:
        let resultEvents = squash sourceEvents
        -- THEN:
        resultEvents `shouldBe` expEvents
    it "squashSimple" $
      -- GIVEN: prepare data
      do
        let sourceEvents =
              [ a_q1
              , a_ch1
              , e_ch1_label_1
              , e_q1_title_1
              , a_ref1
              , a_q2
              , e_q2_title
              , e_q1_text
              , a_q3
              , e_q1_answerUuids
              , e_q1_answerUuids_title
              , e_q1_title_2
              , e_q1_type
              , e_ref1_type
              , e_ref1_url
              , e_ch1_label_2
              ]
        -- AND: prepare expectation
        let expEvents =
              [ a_q1
              , a_ch1
              , e_ch1_label_2 {createdAt = e_ch1_label_1.createdAt}
              , e_q1_text {content = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_text__content {title = e_q1_title_1__content.title}, createdAt = e_q1_title_1.createdAt}
              , a_ref1
              , a_q2
              , e_q2_title
              , a_q3
              , e_q1_answerUuids
              , e_q1_answerUuids_title
              , e_q1_title_2
              , e_q1_type
              , e_ref1_url {createdAt = e_ref1_type.createdAt}
              ]
        -- WHEN:
        let resultEvents = squashSimple sourceEvents
        -- THEN:
        resultEvents `shouldBe` expEvents
    it "squashReorderEvents" $
      -- GIVEN: prepare data
      do
        let sourceEvents =
              [ e_q1_text
              , a_q3
              , e_q1_answerUuids
              , e_q1_answerUuids_title
              , e_q1_title_2
              , e_q1_type
              ]
        -- AND: prepare expectation
        let expEvents =
              [ e_q1_text
              , a_q3
              , e_q1_title_2 {content = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_title_2__content {answerUuids = e_q1_answerUuids_title__content.answerUuids}, createdAt = e_q1_answerUuids.createdAt}
              , e_q1_type
              ]
        -- WHEN:
        let resultEvents = squashReorderEvents sourceEvents
        -- THEN:
        resultEvents `shouldBe` expEvents

-- ---------------------------
--   SCENARIO
-- ---------------------------
--   1  a_q1                     #1 	Add 	Option Question
--   2  a_ch1                    #2 	Add 	Choice
--   3  e_ch1_label_1            #2	  Edit	Choice - title
--   4  e_q1_title_1             #1 	Edit 	Option Question - title
--   5  a_ref1                   #3 	Add 	Resource Page Reference
--   6  a_q2                     #4 	Add 	Option Question
--   7  e_q2_title               #4 	Edit 	Option Question - title
--   8  e_q1_text                #1 	Edit	Option Question - text
--   9  a_q3                     #1 	Add 	Option Question
--   10 e_q1_answerUuids         #1 	Edit	Option Question - answerUuids
--   11 e_q1_answerUuids_title   #1 	Edit	Option Question - title & answerUuids
--   12 e_q1_title_2             #1 	Edit	Option Question - title
--   13 e_q1_type                #1	  Edit	Option Question -> Value Question
--   14 e_ref1_type              #3	  Edit	Resource Page Reference -> URL Reference
--   15 e_ref1_url               #3	  Edit	URL Reference - url
--   16 e_ch1_label_2            #2	  Edit	Choice - label
--   17 e_ch1_label_3_day2       #2	  Edit	Choice - label
a_q1 =
  KnowledgeModelEvent
    { uuid = u' "867841b2-a62d-49cd-b933-fc75dddb6c11"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0001"
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = ""
              , text = Nothing
              , requiredPhaseUuid = Nothing
              , annotations = []
              , tagUuids = []
              }
    , createdAt = dt'' 2018 1 1 1
    }

a_ch1 =
  KnowledgeModelEvent
    { uuid = u' "93bc617f-a407-4c08-8c09-627653f2de68"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0002"
    , content =
        AddChoiceEvent'
          AddChoiceEvent
            { aLabel = ""
            , annotations = []
            }
    , createdAt = dt'' 2018 1 1 2
    }

e_ch1_label_1 =
  KnowledgeModelEvent
    { uuid = u' "b91ac5bf-dfb1-4dfa-a1fc-1d9991c334d0"
    , parentUuid = a_ch1.parentUuid
    , entityUuid = a_ch1.entityUuid
    , content =
        EditChoiceEvent'
          EditChoiceEvent
            { aLabel = ChangedValue "Label 1 - 1"
            , annotations = NothingChanged
            }
    , createdAt = dt'' 2018 1 1 3
    }

e_q1_title_1 =
  KnowledgeModelEvent
    { uuid = u' "4023c0d1-8387-4336-89a8-440bd268dd25"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , content = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_title_1__content
    , createdAt = dt'' 2018 1 1 4
    }

e_q1_title_1__content =
  EditOptionsQuestionEvent
    { title = ChangedValue "Question 1 Title - 1"
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = NothingChanged
    }

a_ref1 =
  KnowledgeModelEvent
    { uuid = u' "5d0eddba-ae27-47c2-81f6-ed5c892db884"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0003"
    , content =
        AddReferenceEvent' $
          AddResourcePageReferenceEvent' $
            AddResourcePageReferenceEvent
              { resourcePageUuid = Nothing
              , annotations = []
              }
    , createdAt = dt'' 2018 1 1 5
    }

a_q2 =
  KnowledgeModelEvent
    { uuid = u' "7aac99fe-3710-4a63-9b56-260919029130"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0004"
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = ""
              , text = Nothing
              , requiredPhaseUuid = Nothing
              , annotations = []
              , tagUuids = []
              }
    , createdAt = dt'' 2018 1 1 6
    }

e_q2_title =
  KnowledgeModelEvent
    { uuid = u' "d007a145-b1a3-4779-a6fd-2e4b328489a0"
    , parentUuid = a_q2.parentUuid
    , entityUuid = a_q2.entityUuid
    , content =
        EditQuestionEvent' $
          EditOptionsQuestionEvent' $
            EditOptionsQuestionEvent
              { title = ChangedValue "Question 2 Title"
              , text = NothingChanged
              , requiredPhaseUuid = NothingChanged
              , annotations = NothingChanged
              , tagUuids = NothingChanged
              , expertUuids = NothingChanged
              , referenceUuids = NothingChanged
              , answerUuids = NothingChanged
              }
    , createdAt = dt'' 2018 1 1 7
    }

e_q1_text =
  KnowledgeModelEvent
    { uuid = u' "fa2e6fe0-68af-426d-865d-3cadaf63ce34"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , content = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_text__content
    , createdAt = dt'' 2018 1 1 8
    }

e_q1_text__content =
  EditOptionsQuestionEvent
    { title = NothingChanged
    , text = ChangedValue (Just "Question 1 Text")
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = NothingChanged
    }

a_q3 =
  KnowledgeModelEvent
    { uuid = u' "f534b150-4794-42f5-afbc-5a78e1e48bf0"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0005"
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = ""
              , text = Nothing
              , requiredPhaseUuid = Nothing
              , annotations = []
              , tagUuids = []
              }
    , createdAt = dt'' 2018 1 1 9
    }

e_q1_answerUuids =
  KnowledgeModelEvent
    { uuid = u' "b71b8844-3f70-4a16-bb5a-9a6682068cb9"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , content =
        EditQuestionEvent' $
          EditOptionsQuestionEvent' $
            EditOptionsQuestionEvent
              { title = NothingChanged
              , text = NothingChanged
              , requiredPhaseUuid = NothingChanged
              , annotations = NothingChanged
              , tagUuids = NothingChanged
              , expertUuids = NothingChanged
              , referenceUuids = NothingChanged
              , answerUuids = ChangedValue []
              }
    , createdAt = dt'' 2018 1 1 10
    }

e_q1_answerUuids_title =
  KnowledgeModelEvent
    { uuid = u' "96b14531-fbfa-43e1-af90-1ddc6ab650f5"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , content = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_answerUuids_title__content
    , createdAt = dt'' 2018 1 1 11
    }

e_q1_answerUuids_title__content =
  EditOptionsQuestionEvent
    { title = ChangedValue "Question 1 Title - Question Uuids"
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = ChangedValue []
    }

e_q1_title_2 =
  KnowledgeModelEvent
    { uuid = u' "86e3da04-cb08-41dc-b227-6ef4ece27d99"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , content = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_title_2__content
    , createdAt = dt'' 2018 1 1 12
    }

e_q1_title_2__content =
  EditOptionsQuestionEvent
    { title = ChangedValue "Question 1 Title - 2"
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = NothingChanged
    }

e_q1_type =
  KnowledgeModelEvent
    { uuid = u' "8ccc509f-e02b-46f5-b3be-1eae32a5ce02"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , content =
        EditQuestionEvent' $
          EditValueQuestionEvent' $
            EditValueQuestionEvent
              { title = NothingChanged
              , text = NothingChanged
              , requiredPhaseUuid = NothingChanged
              , annotations = NothingChanged
              , tagUuids = NothingChanged
              , expertUuids = NothingChanged
              , referenceUuids = NothingChanged
              , valueType = NothingChanged
              , validations = NothingChanged
              }
    , createdAt = dt'' 2018 1 1 13
    }

e_ref1_type =
  KnowledgeModelEvent
    { uuid = u' "db18919c-9d75-4178-80fc-189e5a8c2fbb"
    , parentUuid = a_ref1.parentUuid
    , entityUuid = a_ref1.entityUuid
    , content =
        EditReferenceEvent' $
          EditURLReferenceEvent' $
            EditURLReferenceEvent
              { url = NothingChanged
              , aLabel = NothingChanged
              , annotations = NothingChanged
              }
    , createdAt = dt'' 2018 1 1 14
    }

e_ref1_url =
  KnowledgeModelEvent
    { uuid = u' "4b586546-fa4c-4d8a-ab91-513e9483ac4a"
    , parentUuid = a_ref1.parentUuid
    , entityUuid = a_ref1.entityUuid
    , content =
        EditReferenceEvent' $
          EditURLReferenceEvent' $
            EditURLReferenceEvent
              { url = ChangedValue "Url"
              , aLabel = NothingChanged
              , annotations = NothingChanged
              }
    , createdAt = dt'' 2018 1 1 15
    }

e_ch1_label_2 =
  KnowledgeModelEvent
    { uuid = u' "65bb1d5c-f8e5-4ddf-be3b-123024555f43"
    , parentUuid = a_ch1.parentUuid
    , entityUuid = a_ch1.entityUuid
    , content =
        EditChoiceEvent'
          EditChoiceEvent
            { aLabel = ChangedValue "Label 1 - 2"
            , annotations = NothingChanged
            }
    , createdAt = dt'' 2018 1 1 16
    }

e_ch1_label_3_day2 =
  KnowledgeModelEvent
    { uuid = u' "1e1a97e6-cea3-47a8-9de0-135d543a0017"
    , parentUuid = a_ch1.parentUuid
    , entityUuid = a_ch1.entityUuid
    , content =
        EditChoiceEvent'
          EditChoiceEvent
            { aLabel = ChangedValue "Label 1 - 3"
            , annotations = NothingChanged
            }
    , createdAt = dt'' 2018 1 2 1
    }
