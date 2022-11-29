module Wizard.Specs.Service.KnowledgeModel.Squash.SquasherSpec where

import Test.Hspec

import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Model.Event.Event
import Shared.Model.Event.EventField
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Util.Date
import Shared.Util.Uuid
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
              [ a_q1'
              , a_ch1'
              , e_ch1_label_1'
              , e_q1_title_1'
              , a_ref1'
              , a_q2'
              , e_q2_title'
              , e_q1_text'
              , a_q3'
              , e_q1_answerUuids'
              , e_q1_answerUuids_title'
              , e_q1_title_2'
              , e_q1_type'
              , e_ref1_type'
              , e_ref1_url'
              , e_ch1_label_2'
              , e_ch1_label_3_day2'
              ]
        -- AND: prepare expectation
        let expEvents =
              [ a_q1'
              , a_ch1'
              , a_ref1'
              , a_q2'
              , e_q2_title'
              , EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_text {title = e_q1_title_1.title}
              , a_q3'
              , EditQuestionEvent' . EditOptionsQuestionEvent' $
                  e_q1_title_2 {answerUuids = e_q1_answerUuids_title.answerUuids}
              , e_q1_type'
              , e_ref1_url'
              , e_ch1_label_2'
              , e_ch1_label_3_day2'
              ]
        -- WHEN:
        let resultEvents = squash sourceEvents
        -- THEN:
        resultEvents `shouldBe` expEvents
    it "squashSimple" $
      -- GIVEN: prepare data
      do
        let sourceEvents =
              [ a_q1'
              , a_ch1'
              , e_ch1_label_1'
              , e_q1_title_1'
              , a_ref1'
              , a_q2'
              , e_q2_title'
              , e_q1_text'
              , a_q3'
              , e_q1_answerUuids'
              , e_q1_answerUuids_title'
              , e_q1_title_2'
              , e_q1_type'
              , e_ref1_type'
              , e_ref1_url'
              , e_ch1_label_2'
              ]
        -- AND: prepare expectation
        let expEvents =
              [ a_q1'
              , a_ch1'
              , a_ref1'
              , a_q2'
              , e_q2_title'
              , EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_text {title = e_q1_title_1.title}
              , a_q3'
              , e_q1_answerUuids'
              , e_q1_answerUuids_title'
              , e_q1_title_2'
              , e_q1_type'
              , e_ref1_url'
              , e_ch1_label_2'
              ]
        -- WHEN:
        let resultEvents = squashSimple sourceEvents
        -- THEN:
        resultEvents `shouldBe` expEvents
    it "squashReorderEvents" $
      -- GIVEN: prepare data
      do
        let sourceEvents = [e_q1_text', a_q3', e_q1_answerUuids', e_q1_answerUuids_title', e_q1_title_2', e_q1_type']
        -- AND: prepare expectation
        let expEvents =
              [ e_q1_text'
              , a_q3'
              , EditQuestionEvent' . EditOptionsQuestionEvent' $
                  e_q1_title_2 {answerUuids = e_q1_answerUuids_title.answerUuids}
              , e_q1_type'
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
--   14 e_ref1_type              #3	  Edit	Cross Reference -> URL Reference
--   15 e_ref1_url               #3	  Edit	URL Reference - url
--   16 e_ch1_label_2            #2	  Edit	Choice - label
--   17 e_ch1_label_3_day2       #2	  Edit	Choice - label
a_q1' :: Event
a_q1' = AddQuestionEvent' . AddOptionsQuestionEvent' $ a_q1

a_q1 =
  AddOptionsQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0001"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0001"
    , title = ""
    , text = Nothing
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = []
    , createdAt = dt'' 2018 1 1 1
    }

a_ch1' :: Event
a_ch1' = AddChoiceEvent' a_ch1

a_ch1 =
  AddChoiceEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0002"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0002"
    , aLabel = ""
    , annotations = []
    , createdAt = dt'' 2018 1 1 2
    }

e_ch1_label_1' :: Event
e_ch1_label_1' = EditChoiceEvent' e_ch1_label_1

e_ch1_label_1 =
  EditChoiceEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0003"
    , parentUuid = a_ch1.parentUuid
    , entityUuid = a_ch1.entityUuid
    , aLabel = ChangedValue "Label 1 - 1"
    , annotations = NothingChanged
    , createdAt = dt'' 2018 1 1 3
    }

e_q1_title_1' :: Event
e_q1_title_1' = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_title_1

e_q1_title_1 =
  EditOptionsQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0004"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , title = ChangedValue "Question 1 Title - 1"
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = NothingChanged
    , createdAt = dt'' 2018 1 1 4
    }

a_ref1' :: Event
a_ref1' = AddReferenceEvent' . AddResourcePageReferenceEvent' $ a_ref1

a_ref1 =
  AddResourcePageReferenceEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0005"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0003"
    , shortUuid = ""
    , annotations = []
    , createdAt = dt'' 2018 1 1 5
    }

a_q2' :: Event
a_q2' = AddQuestionEvent' . AddOptionsQuestionEvent' $ a_q2

a_q2 =
  AddOptionsQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0006"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0004"
    , title = ""
    , text = Nothing
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = []
    , createdAt = dt'' 2018 1 1 6
    }

e_q2_title' :: Event
e_q2_title' = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q2_title

e_q2_title =
  EditOptionsQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0007"
    , parentUuid = a_q2.parentUuid
    , entityUuid = a_q2.entityUuid
    , title = ChangedValue "Question 2 Title"
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = NothingChanged
    , createdAt = dt'' 2018 1 1 7
    }

e_q1_text' :: Event
e_q1_text' = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_text

e_q1_text =
  EditOptionsQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0008"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , title = NothingChanged
    , text = ChangedValue (Just "Question 1 Text")
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = NothingChanged
    , createdAt = dt'' 2018 1 1 8
    }

a_q3' :: Event
a_q3' = AddQuestionEvent' . AddOptionsQuestionEvent' $ a_q3

a_q3 =
  AddOptionsQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0009"
    , parentUuid = u' "70c0e4f3-7a67-49dd-8043-e504392d7903"
    , entityUuid = u' "1890b807-83e8-4a20-8515-83930cab0005"
    , title = ""
    , text = Nothing
    , requiredPhaseUuid = Nothing
    , annotations = []
    , tagUuids = []
    , createdAt = dt'' 2018 1 1 9
    }

e_q1_answerUuids' :: Event
e_q1_answerUuids' = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_answerUuids

e_q1_answerUuids =
  EditOptionsQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0010"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , title = NothingChanged
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = ChangedValue []
    , createdAt = dt'' 2018 1 1 10
    }

e_q1_answerUuids_title' :: Event
e_q1_answerUuids_title' = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_answerUuids_title

e_q1_answerUuids_title =
  EditOptionsQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0011"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , title = ChangedValue "Question 1 Title - Question Uuids"
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = ChangedValue []
    , createdAt = dt'' 2018 1 1 11
    }

e_q1_title_2' :: Event
e_q1_title_2' = EditQuestionEvent' . EditOptionsQuestionEvent' $ e_q1_title_2

e_q1_title_2 =
  EditOptionsQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0012"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , title = ChangedValue "Question 1 Title - 2"
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , answerUuids = NothingChanged
    , createdAt = dt'' 2018 1 1 12
    }

e_q1_type' :: Event
e_q1_type' = EditQuestionEvent' . EditValueQuestionEvent' $ e_q1_type

e_q1_type =
  EditValueQuestionEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0013"
    , parentUuid = a_q1.parentUuid
    , entityUuid = a_q1.entityUuid
    , title = NothingChanged
    , text = NothingChanged
    , requiredPhaseUuid = NothingChanged
    , annotations = NothingChanged
    , tagUuids = NothingChanged
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , valueType = NothingChanged
    , createdAt = dt'' 2018 1 1 13
    }

e_ref1_type' :: Event
e_ref1_type' = EditReferenceEvent' . EditURLReferenceEvent' $ e_ref1_type

e_ref1_type =
  EditURLReferenceEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0014"
    , parentUuid = a_ref1.parentUuid
    , entityUuid = a_ref1.entityUuid
    , url = NothingChanged
    , aLabel = NothingChanged
    , annotations = NothingChanged
    , createdAt = dt'' 2018 1 1 14
    }

e_ref1_url' :: Event
e_ref1_url' = EditReferenceEvent' . EditURLReferenceEvent' $ e_ref1_url

e_ref1_url =
  EditURLReferenceEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0015"
    , parentUuid = a_ref1.parentUuid
    , entityUuid = a_ref1.entityUuid
    , url = ChangedValue "Url"
    , aLabel = NothingChanged
    , annotations = NothingChanged
    , createdAt = dt'' 2018 1 1 15
    }

e_ch1_label_2' :: Event
e_ch1_label_2' = EditChoiceEvent' e_ch1_label_2

e_ch1_label_2 =
  EditChoiceEvent
    { uuid = u' "abfcd9db-64df-44c7-96d4-135d543a0016"
    , parentUuid = a_ch1.parentUuid
    , entityUuid = a_ch1.entityUuid
    , aLabel = ChangedValue "Label 1 - 2"
    , annotations = NothingChanged
    , createdAt = dt'' 2018 1 1 16
    }

e_ch1_label_3_day2' :: Event
e_ch1_label_3_day2' = EditChoiceEvent' e_ch1_label_3_day2

e_ch1_label_3_day2 =
  EditChoiceEvent
    { uuid = u' "1e1a97e6-cea3-47a8-9de0-135d543a0017"
    , parentUuid = a_ch1.parentUuid
    , entityUuid = a_ch1.entityUuid
    , aLabel = ChangedValue "Label 1 - 3"
    , annotations = NothingChanged
    , createdAt = dt'' 2018 1 2 1
    }
