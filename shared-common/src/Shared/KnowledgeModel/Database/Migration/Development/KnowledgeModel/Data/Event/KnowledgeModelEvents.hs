module Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents where

import qualified Data.UUID as U

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Choices
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Metrics
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Resources
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Phase.PhaseEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Reference.ReferenceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Resource.ResourceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Tag.TagEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

a_km1 :: KnowledgeModelEvent
a_km1 =
  KnowledgeModelEvent
    { uuid = u' "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f"
    , parentUuid = U.nil
    , entityUuid = km1WithoutChaptersAndTagsAndIntegrations.uuid
    , content =
        AddKnowledgeModelEvent' $
          AddKnowledgeModelEvent
            { annotations = km1WithoutChaptersAndTagsAndIntegrations.annotations
            }
    , createdAt = dt'' 2018 1 21 1
    }

e_km1 :: KnowledgeModelEvent
e_km1 =
  KnowledgeModelEvent
    { uuid = u' "8294a55d-642d-416c-879b-5a42a4430c24"
    , parentUuid = U.nil
    , entityUuid = km1.uuid
    , content = EditKnowledgeModelEvent' e_km1__content
    , createdAt = dt'' 2018 1 21 2
    }

e_km1__content :: EditKnowledgeModelEvent
e_km1__content =
  EditKnowledgeModelEvent
    { annotations = ChangedValue $ km1Edited.annotations
    , chapterUuids = ChangedValue $ km1Edited.chapterUuids
    , tagUuids = ChangedValue $ km1Edited.tagUuids
    , integrationUuids = ChangedValue $ km1Edited.integrationUuids
    , metricUuids = ChangedValue $ km1Edited.metricUuids
    , phaseUuids = ChangedValue $ km1Edited.phaseUuids
    , resourceCollectionUuids = ChangedValue $ km1Edited.resourceCollectionUuids
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1 :: KnowledgeModelEvent
a_km1_ch1 =
  KnowledgeModelEvent
    { uuid = u' "f2e3830c-e2e5-4303-8709-28ad8bdfefa9"
    , parentUuid = km1.uuid
    , entityUuid = chapter1WithoutQuestions.uuid
    , content =
        AddChapterEvent' $
          AddChapterEvent
            { title = chapter1WithoutQuestions.title
            , text = chapter1WithoutQuestions.text
            , annotations = chapter1WithoutQuestions.annotations
            }
    , createdAt = dt'' 2018 1 21 23
    }

a_km1_ch2 :: KnowledgeModelEvent
a_km1_ch2 =
  KnowledgeModelEvent
    { uuid = u' "6c4bba6e-864b-4871-98ca-49ac7a3e5eb5"
    , parentUuid = km1.uuid
    , entityUuid = chapter2WithoutQuestions.uuid
    , content =
        AddChapterEvent' $
          AddChapterEvent
            { title = chapter2WithoutQuestions.title
            , text = chapter2WithoutQuestions.text
            , annotations = chapter2WithoutQuestions.annotations
            }
    , createdAt = dt'' 2018 1 21 38
    }

a_km1_ch3 :: KnowledgeModelEvent
a_km1_ch3 =
  KnowledgeModelEvent
    { uuid = u' "6eaa2b47-711d-4187-98f8-fccdce94db9b"
    , parentUuid = km1.uuid
    , entityUuid = chapter3.uuid
    , content =
        AddChapterEvent' $
          AddChapterEvent
            { title = chapter3.title
            , text = chapter3.text
            , annotations = chapter3.annotations
            }
    , createdAt = dt'' 2018 1 21 57
    }

a_km1_ch4 :: KnowledgeModelEvent
a_km1_ch4 =
  KnowledgeModelEvent
    { uuid = u' "6585a64d-c75b-47fc-a86e-e0c8e773528f"
    , parentUuid = km1.uuid
    , entityUuid = chapter4WithoutQuestions.uuid
    , content =
        AddChapterEvent' $
          AddChapterEvent
            { title = chapter4WithoutQuestions.title
            , text = chapter4WithoutQuestions.text
            , annotations = chapter4WithoutQuestions.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1 :: KnowledgeModelEvent
e_km1_ch1 =
  KnowledgeModelEvent
    { uuid = u' "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
    , parentUuid = km1.uuid
    , entityUuid = chapter1.uuid
    , content = EditChapterEvent' e_km1_ch1__content
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1__content :: EditChapterEvent
e_km1_ch1__content =
  EditChapterEvent
    { title = ChangedValue $ chapter1Edited.title
    , text = ChangedValue $ chapter1Edited.text
    , annotations = ChangedValue $ chapter1Edited.annotations
    , questionUuids = ChangedValue $ chapter1Edited.questionUuids
    }

e_km1_ch1_2 :: KnowledgeModelEvent
e_km1_ch1_2 =
  KnowledgeModelEvent
    { uuid = u' "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
    , parentUuid = km1.uuid
    , entityUuid = chapter1.uuid
    , content =
        EditChapterEvent' $
          EditChapterEvent
            { title = ChangedValue $ "TWICE: " ++ chapter1Edited.title
            , text = ChangedValue $ chapter1Edited.text
            , annotations = ChangedValue $ chapter1Edited.annotations
            , questionUuids = ChangedValue $ chapter1Edited.questionUuids
            }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_ch1 :: KnowledgeModelEvent
d_km1_ch1 =
  KnowledgeModelEvent
    { uuid = u' "d07cc69b-abd3-43ec-bce1-fe59899dbda3"
    , parentUuid = km1.uuid
    , entityUuid = chapter1.uuid
    , content = DeleteChapterEvent' DeleteChapterEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q1 :: KnowledgeModelEvent
a_km1_ch1_q1 =
  KnowledgeModelEvent
    { uuid = u' "71ae2ce9-553b-4ca2-a542-1bce04406c51"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , content =
        AddQuestionEvent' $
          AddValueQuestionEvent' $
            AddValueQuestionEvent
              { title = question1.title
              , text = question1.text
              , requiredPhaseUuid = question1.requiredPhaseUuid
              , annotations = question1.annotations
              , tagUuids = question1.tagUuids
              , valueType = question1.valueType
              , validations = question1.validations
              }
    , createdAt = dt'' 2018 1 21 24
    }

a_km1_ch1_q2 :: KnowledgeModelEvent
a_km1_ch1_q2 =
  KnowledgeModelEvent
    { uuid = u' "ced9be29-24af-4443-8f5f-e709791a8fe3"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = question2.title
              , text = question2.text
              , requiredPhaseUuid = question2.requiredPhaseUuid
              , annotations = question2.annotations
              , tagUuids = question2.tagUuids
              }
    , createdAt = dt'' 2018 1 21 25
    }

a_km1_ch1_q3 :: KnowledgeModelEvent
a_km1_ch1_q3 =
  KnowledgeModelEvent
    { uuid = u' "d559ac95-cc81-4502-a780-dbaee46f24bc"
    , parentUuid = chapter1.uuid
    , entityUuid = question3.uuid
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = question3.title
              , text = question3.text
              , requiredPhaseUuid = question3.requiredPhaseUuid
              , annotations = question3.annotations
              , tagUuids = question3.tagUuids
              }
    , createdAt = dt'' 2018 1 21 0
    }

a_km1_ch2_q3 :: KnowledgeModelEvent
a_km1_ch2_q3 =
  KnowledgeModelEvent
    { uuid = u' "bc994b0f-bee1-4f28-9945-9714b0e559e9"
    , parentUuid = chapter2.uuid
    , entityUuid = question3.uuid
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = question3.title
              , text = question3.text
              , requiredPhaseUuid = question3.requiredPhaseUuid
              , annotations = question3.annotations
              , tagUuids = question3.tagUuids
              }
    , createdAt = dt'' 2018 1 21 39
    }

a_km1_ch2_q4 :: KnowledgeModelEvent
a_km1_ch2_q4 =
  KnowledgeModelEvent
    { uuid = u' "237f1738-af24-4504-b840-5b34eda11324"
    , parentUuid = chapter2.uuid
    , entityUuid = question4.uuid
    , content =
        AddQuestionEvent' $
          AddListQuestionEvent'
            AddListQuestionEvent
              { title = question4.title
              , text = question4.text
              , requiredPhaseUuid = question4.requiredPhaseUuid
              , annotations = question4.annotations
              , tagUuids = question4.tagUuids
              }
    , createdAt = dt'' 2018 1 21 42
    }

a_km1_ch3_q9 :: KnowledgeModelEvent
a_km1_ch3_q9 =
  KnowledgeModelEvent
    { uuid = u' "51526318-2727-4113-993d-bae5d4abafcd"
    , parentUuid = chapter3.uuid
    , entityUuid = question9.uuid
    , content =
        AddQuestionEvent' $
          AddIntegrationQuestionEvent' $
            AddIntegrationQuestionEvent
              { title = question9.title
              , text = question9.text
              , requiredPhaseUuid = question9.requiredPhaseUuid
              , annotations = question9.annotations
              , tagUuids = question9.tagUuids
              , integrationUuid = question9.integrationUuid
              , variables = question9.variables
              }
    , createdAt = dt'' 2018 1 21 58
    }

a_km1_ch3_q10 :: KnowledgeModelEvent
a_km1_ch3_q10 =
  KnowledgeModelEvent
    { uuid = u' "e8531168-946d-4d95-a3b5-f092d32dee1a"
    , parentUuid = chapter3.uuid
    , entityUuid = question10.uuid
    , content =
        AddQuestionEvent' $
          AddIntegrationQuestionEvent' $
            AddIntegrationQuestionEvent
              { title = question10.title
              , text = question10.text
              , requiredPhaseUuid = question10.requiredPhaseUuid
              , tagUuids = question10.tagUuids
              , integrationUuid = question10.integrationUuid
              , annotations = question10.annotations
              , variables = question10.variables
              }
    , createdAt = dt'' 2018 1 21 59
    }

a_km1_ch3_q11 :: KnowledgeModelEvent
a_km1_ch3_q11 =
  KnowledgeModelEvent
    { uuid = u' "2083c6d2-6fa4-4170-8b14-5f5a518b78b2"
    , parentUuid = chapter3.uuid
    , entityUuid = question11.uuid
    , content =
        AddQuestionEvent' $
          AddMultiChoiceQuestionEvent' $
            AddMultiChoiceQuestionEvent
              { title = question11.title
              , text = question11.text
              , requiredPhaseUuid = question11.requiredPhaseUuid
              , annotations = question11.annotations
              , tagUuids = question11.tagUuids
              }
    , createdAt = dt'' 2018 1 21 60
    }

a_km1_ch3_q12 :: KnowledgeModelEvent
a_km1_ch3_q12 =
  KnowledgeModelEvent
    { uuid = u' "e5e6eb01-f55f-422b-9423-ada60f55b36c"
    , parentUuid = chapter3.uuid
    , entityUuid = question12.uuid
    , content =
        AddQuestionEvent' $
          AddMultiChoiceQuestionEvent' $
            AddMultiChoiceQuestionEvent
              { title = question12.title
              , text = question12.text
              , requiredPhaseUuid = question12.requiredPhaseUuid
              , annotations = question12.annotations
              , tagUuids = question12.tagUuids
              }
    , createdAt = dt'' 2018 1 21 63
    }

a_km1_ch3_q13 :: KnowledgeModelEvent
a_km1_ch3_q13 =
  KnowledgeModelEvent
    { uuid = u' "49039f8c-3389-4da3-a4a9-8d2c052b38cd"
    , parentUuid = chapter3.uuid
    , entityUuid = question13.uuid
    , content =
        AddQuestionEvent' $
          AddItemSelectQuestionEvent' $
            AddItemSelectQuestionEvent
              { title = question13.title
              , text = question13.text
              , requiredPhaseUuid = question13.requiredPhaseUuid
              , annotations = question13.annotations
              , tagUuids = question13.tagUuids
              , listQuestionUuid = question13.listQuestionUuid
              }
    , createdAt = dt'' 2018 1 21 64
    }

a_km1_ch3_q14 :: KnowledgeModelEvent
a_km1_ch3_q14 =
  KnowledgeModelEvent
    { uuid = u' "cdd54bfa-b2de-4ab3-9022-bc512cfa297f"
    , parentUuid = chapter3.uuid
    , entityUuid = question14.uuid
    , content =
        AddQuestionEvent' $
          AddFileQuestionEvent' $
            AddFileQuestionEvent
              { title = question14.title
              , text = question14.text
              , requiredPhaseUuid = question14.requiredPhaseUuid
              , annotations = question14.annotations
              , tagUuids = question14.tagUuids
              , maxSize = question14.maxSize
              , fileTypes = question14.fileTypes
              }
    , createdAt = dt'' 2018 1 21 65
    }

a_km1_ch3_q15 :: KnowledgeModelEvent
a_km1_ch3_q15 =
  KnowledgeModelEvent
    { uuid = u' "961ee868-9c87-4078-99ef-01719f8bd63e"
    , parentUuid = chapter3.uuid
    , entityUuid = question15.uuid
    , content =
        AddQuestionEvent' $
          AddIntegrationQuestionEvent' $
            AddIntegrationQuestionEvent
              { title = question15.title
              , text = question15.text
              , requiredPhaseUuid = question15.requiredPhaseUuid
              , annotations = question15.annotations
              , tagUuids = question15.tagUuids
              , integrationUuid = question15.integrationUuid
              , variables = question15.variables
              }
    , createdAt = dt'' 2018 1 21 66
    }

e_km1_ch1_q1 :: KnowledgeModelEvent
e_km1_ch1_q1 =
  KnowledgeModelEvent
    { uuid = u' "de86f82b-aaaf-482e-97c7-c7e93d834cd9"
    , parentUuid = chapter1.uuid
    , entityUuid = question1Edited.uuid
    , content =
        EditQuestionEvent' $
          EditValueQuestionEvent' $
            EditValueQuestionEvent
              { title = ChangedValue question1Edited.title
              , text = NothingChanged
              , requiredPhaseUuid = NothingChanged
              , annotations = ChangedValue question1Edited.annotations
              , tagUuids = NothingChanged
              , expertUuids = NothingChanged
              , referenceUuids = NothingChanged
              , valueType = NothingChanged
              , validations = ChangedValue question1Edited.validations
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q1_type :: KnowledgeModelEvent
e_km1_ch1_q1_type =
  KnowledgeModelEvent
    { uuid = u' "f56b1435-ec9f-4d79-88b3-04c39b73724d"
    , parentUuid = chapter1.uuid
    , entityUuid = question1WithNewType.uuid
    , content =
        EditQuestionEvent' $
          EditOptionsQuestionEvent' $
            EditOptionsQuestionEvent
              { title = ChangedValue question1WithNewType.title
              , text = NothingChanged
              , requiredPhaseUuid = NothingChanged
              , annotations = ChangedValue question1WithNewType.annotations
              , tagUuids = NothingChanged
              , expertUuids = NothingChanged
              , referenceUuids = NothingChanged
              , answerUuids = ChangedValue $ question1WithNewType.answerUuids
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2 :: KnowledgeModelEvent
e_km1_ch1_q2 =
  KnowledgeModelEvent
    { uuid = u' "1a01665b-e896-450d-b606-afc1dcca586b"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , content = EditQuestionEvent' $ EditOptionsQuestionEvent' e_km1_ch1_q2__content
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2__content :: EditOptionsQuestionEvent
e_km1_ch1_q2__content =
  EditOptionsQuestionEvent
    { title = ChangedValue $ question2Edited.title
    , text = ChangedValue $ question2Edited.text
    , requiredPhaseUuid = ChangedValue $ question2Edited.requiredPhaseUuid
    , annotations = ChangedValue $ question2Edited.annotations
    , tagUuids = ChangedValue $ question2Edited.tagUuids
    , expertUuids = ChangedValue $ question2Edited.expertUuids
    , referenceUuids = ChangedValue $ question2Edited.referenceUuids
    , answerUuids = ChangedValue $ question2Edited.answerUuids
    }

e_km1_ch1_q2_second_edit :: KnowledgeModelEvent
e_km1_ch1_q2_second_edit =
  KnowledgeModelEvent
    { uuid = u' "bf888b95-921d-4caa-88af-3309393d44c3"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , content =
        EditQuestionEvent' $
          EditOptionsQuestionEvent' $
            EditOptionsQuestionEvent
              { title = ChangedValue "New title"
              , text = ChangedValue $ question2Edited.text
              , requiredPhaseUuid = ChangedValue $ question2Edited.requiredPhaseUuid
              , annotations = ChangedValue $ question2Edited.annotations
              , tagUuids = ChangedValue $ question2Edited.tagUuids
              , expertUuids = ChangedValue $ question2Edited.expertUuids
              , referenceUuids = ChangedValue $ question2Edited.referenceUuids
              , answerUuids = ChangedValue $ question2Edited.answerUuids
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_type :: KnowledgeModelEvent
e_km1_ch1_q2_type =
  KnowledgeModelEvent
    { uuid = u' "2727c225-78e5-4d5f-a093-cfaadb6ea663"
    , parentUuid = chapter1.uuid
    , entityUuid = question2WithNewType.uuid
    , content =
        EditQuestionEvent' $
          EditListQuestionEvent' $
            EditListQuestionEvent
              { title = ChangedValue $ question2WithNewType.title
              , text = NothingChanged
              , requiredPhaseUuid = NothingChanged
              , annotations = ChangedValue $ question2WithNewType.annotations
              , tagUuids = NothingChanged
              , expertUuids = NothingChanged
              , referenceUuids = NothingChanged
              , itemTemplateQuestionUuids = ChangedValue []
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch2_q4 :: KnowledgeModelEvent
e_km1_ch2_q4 =
  KnowledgeModelEvent
    { uuid = u' "7014c6de-a1c0-4c09-881a-c83c68a29de1"
    , parentUuid = chapter2.uuid
    , entityUuid = question4Edited.uuid
    , content =
        EditQuestionEvent' $
          EditListQuestionEvent' $
            EditListQuestionEvent
              { title = ChangedValue $ question4Edited.title
              , text = ChangedValue $ question4Edited.text
              , requiredPhaseUuid = ChangedValue $ question4Edited.requiredPhaseUuid
              , annotations = ChangedValue $ question4Edited.annotations
              , tagUuids = ChangedValue $ question4Edited.tagUuids
              , expertUuids = ChangedValue $ question4Edited.expertUuids
              , referenceUuids = ChangedValue $ question4Edited.referenceUuids
              , itemTemplateQuestionUuids = ChangedValue $ question4Edited.itemTemplateQuestionUuids
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch2_q4_type :: KnowledgeModelEvent
e_km1_ch2_q4_type =
  KnowledgeModelEvent
    { uuid = u' "0f6f536c-aa1c-4d47-8cd7-46d611b43a56"
    , parentUuid = chapter2.uuid
    , entityUuid = question4WithNewType.uuid
    , content =
        EditQuestionEvent' $
          EditIntegrationQuestionEvent' $
            EditIntegrationQuestionEvent
              { title = ChangedValue $ question4WithNewType.title
              , text = NothingChanged
              , requiredPhaseUuid = NothingChanged
              , annotations = ChangedValue $ question4WithNewType.annotations
              , tagUuids = NothingChanged
              , expertUuids = NothingChanged
              , referenceUuids = NothingChanged
              , integrationUuid = ChangedValue $ question4WithNewType.integrationUuid
              , variables = ChangedValue $ question4WithNewType.variables
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch3_q9 :: KnowledgeModelEvent
e_km1_ch3_q9 =
  KnowledgeModelEvent
    { uuid = u' "43779823-507b-41f1-8dce-7c5e0660db8f"
    , parentUuid = chapter3.uuid
    , entityUuid = question9Edited.uuid
    , content =
        EditQuestionEvent' $
          EditIntegrationQuestionEvent' $
            EditIntegrationQuestionEvent
              { title = ChangedValue $ question9Edited.title
              , text = ChangedValue $ question9Edited.text
              , requiredPhaseUuid = ChangedValue $ question9Edited.requiredPhaseUuid
              , annotations = ChangedValue $ question9Edited.annotations
              , tagUuids = ChangedValue $ question9Edited.tagUuids
              , expertUuids = ChangedValue $ question9Edited.expertUuids
              , referenceUuids = ChangedValue $ question9Edited.referenceUuids
              , integrationUuid = ChangedValue $ question9Edited.integrationUuid
              , variables = ChangedValue $ question9Edited.variables
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch3_q9_type :: KnowledgeModelEvent
e_km1_ch3_q9_type =
  KnowledgeModelEvent
    { uuid = u' "91514dc3-29b1-469a-b0d9-5fc211df1c47"
    , parentUuid = chapter3.uuid
    , entityUuid = question9WithNewType.uuid
    , content =
        EditQuestionEvent' $
          EditValueQuestionEvent' $
            EditValueQuestionEvent
              { title = ChangedValue $ question9WithNewType.title
              , text = NothingChanged
              , requiredPhaseUuid = NothingChanged
              , annotations = ChangedValue $ question9WithNewType.annotations
              , tagUuids = NothingChanged
              , expertUuids = NothingChanged
              , referenceUuids = NothingChanged
              , valueType = ChangedValue $ question9WithNewType.valueType
              , validations = NothingChanged
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch3_q11 :: KnowledgeModelEvent
e_km1_ch3_q11 =
  KnowledgeModelEvent
    { uuid = u' "1a01665b-e896-450d-b606-afc1dcca586b"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , content =
        EditQuestionEvent' $
          EditMultiChoiceQuestionEvent' $
            EditMultiChoiceQuestionEvent
              { title = ChangedValue $ question11Edited.title
              , text = ChangedValue $ question11Edited.text
              , requiredPhaseUuid = ChangedValue $ question11Edited.requiredPhaseUuid
              , annotations = ChangedValue $ question11Edited.annotations
              , tagUuids = ChangedValue $ question11Edited.tagUuids
              , expertUuids = ChangedValue $ question11Edited.expertUuids
              , referenceUuids = ChangedValue $ question11Edited.referenceUuids
              , choiceUuids = ChangedValue $ question11Edited.choiceUuids
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch3_q13 :: KnowledgeModelEvent
e_km1_ch3_q13 =
  KnowledgeModelEvent
    { uuid = u' "f95ace16-9a2c-4f66-b22f-e419e5014c58"
    , parentUuid = chapter3.uuid
    , entityUuid = question13Edited.uuid
    , content =
        EditQuestionEvent' $
          EditItemSelectQuestionEvent' $
            EditItemSelectQuestionEvent
              { title = ChangedValue $ question13Edited.title
              , text = ChangedValue $ question13Edited.text
              , requiredPhaseUuid = ChangedValue $ question13Edited.requiredPhaseUuid
              , annotations = ChangedValue $ question13Edited.annotations
              , tagUuids = ChangedValue $ question13Edited.tagUuids
              , expertUuids = ChangedValue $ question13Edited.expertUuids
              , referenceUuids = ChangedValue $ question13Edited.referenceUuids
              , listQuestionUuid = ChangedValue $ question13Edited.listQuestionUuid
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch3_q14 :: KnowledgeModelEvent
e_km1_ch3_q14 =
  KnowledgeModelEvent
    { uuid = u' "cd144dd9-c722-400f-9972-2642177dee06"
    , parentUuid = chapter3.uuid
    , entityUuid = question14Edited.uuid
    , content =
        EditQuestionEvent' $
          EditFileQuestionEvent' $
            EditFileQuestionEvent
              { title = ChangedValue question14Edited.title
              , text = ChangedValue question14Edited.text
              , requiredPhaseUuid = ChangedValue question14Edited.requiredPhaseUuid
              , annotations = ChangedValue question14Edited.annotations
              , tagUuids = ChangedValue question14Edited.tagUuids
              , expertUuids = ChangedValue question14Edited.expertUuids
              , referenceUuids = ChangedValue question14Edited.referenceUuids
              , maxSize = ChangedValue question14Edited.maxSize
              , fileTypes = ChangedValue question14Edited.fileTypes
              }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_ch1_q1 :: KnowledgeModelEvent
d_km1_ch1_q1 =
  KnowledgeModelEvent
    { uuid = u' "aed9cf13-c81a-481f-bd8a-2689c4a74369"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , content = DeleteQuestionEvent' DeleteQuestionEvent
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_ch1_q1_2 :: KnowledgeModelEvent
d_km1_ch1_q1_2 =
  KnowledgeModelEvent
    { uuid = u' "aed9cf13-c81a-481f-bd8a-2689c4a74369"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , content = DeleteQuestionEvent' DeleteQuestionEvent
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_ch1_q2 :: KnowledgeModelEvent
d_km1_ch1_q2 =
  KnowledgeModelEvent
    { uuid = u' "52a7a6ae-be37-4075-ac5c-a20858707a75"
    , parentUuid = chapter1.uuid
    , entityUuid = question2.uuid
    , content = DeleteQuestionEvent' DeleteQuestionEvent
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_ch1_q3 :: KnowledgeModelEvent
d_km1_ch1_q3 =
  KnowledgeModelEvent
    { uuid = u' "e46d208f-eb7d-48bc-8187-13a72b17ddb2"
    , parentUuid = chapter1.uuid
    , entityUuid = question3.uuid
    , content = DeleteQuestionEvent' DeleteQuestionEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_aNo1 :: KnowledgeModelEvent
a_km1_ch1_q2_aNo1 =
  KnowledgeModelEvent
    { uuid = u' "afb36736-503a-43ca-a56b-8c144f89809e"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerNo.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q2_answerNo.aLabel
            , advice = q2_answerNo.advice
            , annotations = q2_answerNo.annotations
            , metricMeasures = q2_answerNo.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 26
    }

a_km1_ch1_q2_aYes1 :: KnowledgeModelEvent
a_km1_ch1_q2_aYes1 =
  KnowledgeModelEvent
    { uuid = u' "e7ee93e4-18e7-4748-b0a5-781c77b8c937"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q2_answerYes.aLabel
            , advice = q2_answerYes.advice
            , annotations = q2_answerYes.annotations
            , metricMeasures = q2_answerYes.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 27
    }

a_km1_ch1_q2_aMaybe :: KnowledgeModelEvent
a_km1_ch1_q2_aMaybe =
  KnowledgeModelEvent
    { uuid = u' "8ba60993-96ac-496b-9b8c-9580bf992cab"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerMaybe.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q2_answerMaybe.aLabel
            , advice = q2_answerMaybe.advice
            , annotations = q2_answerMaybe.annotations
            , metricMeasures = q2_answerMaybe.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 0
    }

a_km1_ch1_q2_aYes1_fuq1_aNo :: KnowledgeModelEvent
a_km1_ch1_q2_aYes1_fuq1_aNo =
  KnowledgeModelEvent
    { uuid = u' "e62168e2-afe5-4e58-8ee7-555594aec23e"
    , parentUuid = q2_aYes_fuQuestion1.uuid
    , entityUuid = q2_aYes_fuq1_answerNo.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q2_aYes_fuq1_answerNo.aLabel
            , advice = q2_aYes_fuq1_answerNo.advice
            , annotations = q2_aYes_fuq1_answerNo.annotations
            , metricMeasures = q2_aYes_fuq1_answerNo.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 29
    }

a_km1_ch1_q2_aYesFu1 :: KnowledgeModelEvent
a_km1_ch1_q2_aYesFu1 =
  KnowledgeModelEvent
    { uuid = u' "bc530681-b45b-4d36-b179-a9cb62a92838"
    , parentUuid = q2_aYes_fuQuestion1.uuid
    , entityUuid = q2_aYes_fuq1_answerYes.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q2_aYes_fuq1_answerYes.aLabel
            , advice = q2_aYes_fuq1_answerYes.advice
            , annotations = q2_aYes_fuq1_answerYes.annotations
            , metricMeasures = q2_aYes_fuq1_answerYes.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 30
    }

a_km1_ch1_q2_aNoFu2 :: KnowledgeModelEvent
a_km1_ch1_q2_aNoFu2 =
  KnowledgeModelEvent
    { uuid = u' "abf67af9-23e0-43fa-a54a-746570882624"
    , parentUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuq2_answerNo.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q2_aYes_fuq1_aYes_fuq2_answerNo.aLabel
            , advice = q2_aYes_fuq1_aYes_fuq2_answerNo.advice
            , annotations = q2_aYes_fuq1_aYes_fuq2_answerNo.annotations
            , metricMeasures = q2_aYes_fuq1_aYes_fuq2_answerNo.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 32
    }

a_km1_ch1_q2_aYesFu2 :: KnowledgeModelEvent
a_km1_ch1_q2_aYesFu2 =
  KnowledgeModelEvent
    { uuid = u' "542c0d28-9ae3-4bbe-8030-92a78b462276"
    , parentUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuq2_answerYes.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q2_aYes_fuq1_aYes_fuq2_answerYes.aLabel
            , advice = q2_aYes_fuq1_aYes_fuq2_answerYes.advice
            , annotations = q2_aYes_fuq1_aYes_fuq2_answerYes.annotations
            , metricMeasures = q2_aYes_fuq1_aYes_fuq2_answerYes.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 33
    }

a_km1_ch2_q3_aNo2 :: KnowledgeModelEvent
a_km1_ch2_q3_aNo2 =
  KnowledgeModelEvent
    { uuid = u' "1bb10e82-33b5-4c98-b1d1-ab5413b5df66"
    , parentUuid = question3.uuid
    , entityUuid = q3_answerNo.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q3_answerNo.aLabel
            , advice = q3_answerNo.advice
            , annotations = q3_answerNo.annotations
            , metricMeasures = q3_answerNo.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 40
    }

a_km1_ch2_q3_aYes2 :: KnowledgeModelEvent
a_km1_ch2_q3_aYes2 =
  KnowledgeModelEvent
    { uuid = u' "885ea1b9-0041-4240-911c-f35a9a6e4cbd"
    , parentUuid = question3.uuid
    , entityUuid = q3_answerYes.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q3_answerYes.aLabel
            , advice = q3_answerYes.advice
            , annotations = q3_answerYes.annotations
            , metricMeasures = q3_answerYes.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 41
    }

a_km1_ch2_q4_it_q6_aNo :: KnowledgeModelEvent
a_km1_ch2_q4_it_q6_aNo =
  KnowledgeModelEvent
    { uuid = u' "c0a67ce5-21b3-47c7-8624-c2da26fb494f"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = q4_it1_q6_answerNo.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q4_it1_q6_answerNo.aLabel
            , advice = q4_it1_q6_answerNo.advice
            , annotations = q4_it1_q6_answerNo.annotations
            , metricMeasures = q4_it1_q6_answerNo.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 47
    }

a_km1_ch2_q4_it_q6_aYes :: KnowledgeModelEvent
a_km1_ch2_q4_it_q6_aYes =
  KnowledgeModelEvent
    { uuid = u' "c5c42f99-613b-4b6c-ae5e-af784f51c483"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = q4_it1_q6_answerYes.uuid
    , content =
        AddAnswerEvent' $
          AddAnswerEvent
            { aLabel = q4_it1_q6_answerYes.aLabel
            , advice = q4_it1_q6_answerYes.advice
            , annotations = q4_it1_q6_answerYes.annotations
            , metricMeasures = q4_it1_q6_answerYes.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 48
    }

e_km1_ch1_q2_aYes1 :: KnowledgeModelEvent
e_km1_ch1_q2_aYes1 =
  KnowledgeModelEvent
    { uuid = u' "8c6632f6-0335-4912-924a-693a87cbe270"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , content =
        EditAnswerEvent' $
          EditAnswerEvent
            { aLabel = ChangedValue $ q2_answerYesEdited.aLabel
            , advice = ChangedValue $ q2_answerYesEdited.advice
            , annotations = ChangedValue $ q2_answerYesEdited.annotations
            , followUpUuids = ChangedValue $ q2_answerYesEdited.followUpUuids
            , metricMeasures = ChangedValue $ q2_answerYesEdited.metricMeasures
            }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_aYes1_2 :: KnowledgeModelEvent
e_km1_ch1_q2_aYes1_2 =
  KnowledgeModelEvent
    { uuid = u' "8c6632f6-0335-4912-924a-693a87cbe270"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , content = EditAnswerEvent' e_km1_ch1_q2_aYes1_2__content
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_aYes1_2__content :: EditAnswerEvent
e_km1_ch1_q2_aYes1_2__content =
  EditAnswerEvent
    { aLabel = ChangedValue $ q2_answerYesEdited.aLabel
    , advice = ChangedValue $ q2_answerYesEdited.advice
    , annotations = ChangedValue $ q2_answerYesEdited.annotations
    , followUpUuids = ChangedValue $ q2_answerYes.followUpUuids
    , metricMeasures = ChangedValue $ q2_answerYes.metricMeasures
    }

d_km1_ch1_q2_aYes1 :: KnowledgeModelEvent
d_km1_ch1_q2_aYes1 =
  KnowledgeModelEvent
    { uuid = u' "1968692f-959a-4d47-b85f-d684eedb3e7f"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , content = DeleteAnswerEvent' DeleteAnswerEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- AnswerItemTemplateQuestionEvent
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch2_q4_it1_q5 :: KnowledgeModelEvent
a_km1_ch2_q4_it1_q5 =
  KnowledgeModelEvent
    { uuid = u' "5619d036-0130-47fa-9553-b73094eecd7e"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question5.uuid
    , content =
        AddQuestionEvent' $
          AddListQuestionEvent'
            AddListQuestionEvent
              { title = q4_it1_question5.title
              , text = q4_it1_question5.text
              , requiredPhaseUuid = q4_it1_question5.requiredPhaseUuid
              , annotations = q4_it1_question5.annotations
              , tagUuids = q4_it1_question5.tagUuids
              }
    , createdAt = dt'' 2018 1 21 43
    }

a_km1_ch2_q4_it1_q6 :: KnowledgeModelEvent
a_km1_ch2_q4_it1_q6 =
  KnowledgeModelEvent
    { uuid = u' "5ac56741-b93a-42f5-9beb-f22100e4342d"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question6.uuid
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = q4_it1_question6.title
              , text = q4_it1_question6.text
              , requiredPhaseUuid = q4_it1_question6.requiredPhaseUuid
              , annotations = q4_it1_question6.annotations
              , tagUuids = q4_it1_question6.tagUuids
              }
    , createdAt = dt'' 2018 1 21 46
    }

a_km1_ch2_q4_it1_q6_fuq4_q1 :: KnowledgeModelEvent
a_km1_ch2_q4_it1_q6_fuq4_q1 =
  KnowledgeModelEvent
    { uuid = u' "55f46913-a953-4318-b72f-673e9f65fb2a"
    , parentUuid = q4_it1_q6_aYes_followUpQuestion4.uuid
    , entityUuid = q4_it1_q6_aYes_fuq4_it_question1.uuid
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = q4_it1_q6_aYes_fuq4_it_question1.title
              , text = q4_it1_q6_aYes_fuq4_it_question1.text
              , requiredPhaseUuid = q4_it1_q6_aYes_fuq4_it_question1.requiredPhaseUuid
              , annotations = q4_it1_q6_aYes_fuq4_it_question1.annotations
              , tagUuids = q4_it1_q6_aYes_fuq4_it_question1.tagUuids
              }
    , createdAt = dt'' 2018 1 21 51
    }

a_km1_ch2_q4_it1_q6_fuq4_q2 :: KnowledgeModelEvent
a_km1_ch2_q4_it1_q6_fuq4_q2 =
  KnowledgeModelEvent
    { uuid = u' "6b9a7c1c-a23e-458a-a1bb-d7500c0ed96e"
    , parentUuid = q4_it1_q6_aYes_followUpQuestion4.uuid
    , entityUuid = q4_it1_q6_aYes_fuq4_it_question2.uuid
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = q4_it1_q6_aYes_fuq4_it_question2.title
              , text = q4_it1_q6_aYes_fuq4_it_question2.text
              , requiredPhaseUuid = q4_it1_q6_aYes_fuq4_it_question2.requiredPhaseUuid
              , annotations = q4_it1_q6_aYes_fuq4_it_question2.annotations
              , tagUuids = q4_it1_q6_aYes_fuq4_it_question2.tagUuids
              }
    , createdAt = dt'' 2018 1 21 52
    }

a_km1_ch2_q4_it1_q7 :: KnowledgeModelEvent
a_km1_ch2_q4_it1_q7 =
  KnowledgeModelEvent
    { uuid = u' "cf839365-91d0-427a-bb99-89de1a125929"
    , parentUuid = q4_it1_question5.uuid
    , entityUuid = q4_it1_q5_it2_question7.uuid
    , content =
        AddQuestionEvent' $
          AddValueQuestionEvent' $
            AddValueQuestionEvent
              { title = q4_it1_q5_it2_question7.title
              , text = q4_it1_q5_it2_question7.text
              , requiredPhaseUuid = q4_it1_q5_it2_question7.requiredPhaseUuid
              , annotations = q4_it1_q5_it2_question7.annotations
              , tagUuids = q4_it1_q5_it2_question7.tagUuids
              , valueType = q4_it1_q5_it2_question7.valueType
              , validations = q4_it1_q5_it2_question7.validations
              }
    , createdAt = dt'' 2018 1 21 44
    }

a_km1_ch2_q4_it1_q8 :: KnowledgeModelEvent
a_km1_ch2_q4_it1_q8 =
  KnowledgeModelEvent
    { uuid = u' "3536a56f-d19c-4aff-ada1-ef7b3a60389d"
    , parentUuid = q4_it1_question5.uuid
    , entityUuid = q4_it1_q5_it2_question8.uuid
    , content =
        AddQuestionEvent' $
          AddValueQuestionEvent' $
            AddValueQuestionEvent
              { title = q4_it1_q5_it2_question8.title
              , text = q4_it1_q5_it2_question8.text
              , requiredPhaseUuid = q4_it1_q5_it2_question8.requiredPhaseUuid
              , annotations = q4_it1_q5_it2_question8.annotations
              , tagUuids = q4_it1_q5_it2_question8.tagUuids
              , valueType = q4_it1_q5_it2_question8.valueType
              , validations = q4_it1_q5_it2_question8.validations
              }
    , createdAt = dt'' 2018 1 21 45
    }

e_km1_ch2_q4_it1_q5 :: KnowledgeModelEvent
e_km1_ch2_q4_it1_q5 =
  KnowledgeModelEvent
    { uuid = u' "17f8e9d4-7299-4c88-aba1-0a7b133aa8f3"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question5Edited.uuid
    , content = EditQuestionEvent' $ EditListQuestionEvent' e_km1_ch2_q4_it1_q5__content
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch2_q4_it1_q5__content :: EditListQuestionEvent
e_km1_ch2_q4_it1_q5__content =
  EditListQuestionEvent
    { title = ChangedValue $ q4_it1_question5Edited.title
    , text = ChangedValue $ q4_it1_question5Edited.text
    , requiredPhaseUuid = ChangedValue $ q4_it1_question5Edited.requiredPhaseUuid
    , annotations = ChangedValue $ q4_it1_question5Edited.annotations
    , tagUuids = ChangedValue $ q4_it1_question5Edited.tagUuids
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , itemTemplateQuestionUuids = ChangedValue [q4_it1_q5_it2_question8.uuid, q4_it1_q5_it2_question7.uuid]
    }

e_km1_ch2_q4_it1_q6 :: KnowledgeModelEvent
e_km1_ch2_q4_it1_q6 =
  KnowledgeModelEvent
    { uuid = u' "f5c5ccfd-619b-4110-807a-39ede6d31cae"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question6Edited.uuid
    , content = EditQuestionEvent' $ EditOptionsQuestionEvent' e_km1_ch2_q4_it1_q6__content
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch2_q4_it1_q6__content :: EditOptionsQuestionEvent
e_km1_ch2_q4_it1_q6__content =
  EditOptionsQuestionEvent
    { title = ChangedValue $ q4_it1_question6Edited.title
    , text = ChangedValue $ q4_it1_question6Edited.text
    , requiredPhaseUuid = ChangedValue $ q4_it1_question6Edited.requiredPhaseUuid
    , annotations = ChangedValue $ q4_it1_question6Edited.annotations
    , tagUuids = ChangedValue $ q4_it1_question6Edited.tagUuids
    , expertUuids = ChangedValue $ q4_it1_question6Edited.expertUuids
    , referenceUuids = ChangedValue $ q4_it1_question6Edited.referenceUuids
    , answerUuids = ChangedValue $ q4_it1_question6Edited.answerUuids
    }

d_km1_ch2_q4_it1_q5 :: KnowledgeModelEvent
d_km1_ch2_q4_it1_q5 =
  KnowledgeModelEvent
    { uuid = u' "424d19cb-a79f-4da0-b7f6-33363c32b7fd"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question5.uuid
    , content = DeleteQuestionEvent' DeleteQuestionEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- FollowUpQuestionEvent
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_ansYes1_fuq1 :: KnowledgeModelEvent
a_km1_ch1_ansYes1_fuq1 =
  KnowledgeModelEvent
    { uuid = u' "3588358c-159e-41a9-9847-262611007b61"
    , parentUuid = q2_answerYes.uuid
    , entityUuid = q2_aYes_fuQuestion1.uuid
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent'
            AddOptionsQuestionEvent
              { title = q2_aYes_fuQuestion1.title
              , text = q2_aYes_fuQuestion1.text
              , requiredPhaseUuid = q2_aYes_fuQuestion1.requiredPhaseUuid
              , annotations = q2_aYes_fuQuestion1.annotations
              , tagUuids = q2_aYes_fuQuestion1.tagUuids
              }
    , createdAt = dt'' 2018 1 21 28
    }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 :: KnowledgeModelEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2 =
  KnowledgeModelEvent
    { uuid = u' "8ced5634-a879-4da2-b7c9-158ca6a4e0e3"
    , parentUuid = q2_aYes_fuq1_answerYes.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent'
            AddOptionsQuestionEvent
              { title = q2_aYes_fuq1_aYes_fuQuestion2.title
              , text = q2_aYes_fuq1_aYes_fuQuestion2.text
              , requiredPhaseUuid = q2_aYes_fuq1_aYes_fuQuestion2.requiredPhaseUuid
              , annotations = q2_aYes_fuq1_aYes_fuQuestion2.annotations
              , tagUuids = q2_aYes_fuq1_aYes_fuQuestion2.tagUuids
              }
    , createdAt = dt'' 2018 1 21 31
    }

a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 :: KnowledgeModelEvent
a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3 =
  KnowledgeModelEvent
    { uuid = u' "6e9b591f-e6f9-46dd-85e8-a90fe4acc51c"
    , parentUuid = q2_aYes_fuq1_aYes_fuq2_answerYes.uuid
    , entityUuid = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.uuid
    , content =
        AddQuestionEvent' $
          AddOptionsQuestionEvent' $
            AddOptionsQuestionEvent
              { title = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.title
              , text = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.text
              , requiredPhaseUuid = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.requiredPhaseUuid
              , annotations = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.annotations
              , tagUuids = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.tagUuids
              }
    , createdAt = dt'' 2018 1 21 0
    }

a_km1_ch2_ansYes6_fuq4 :: KnowledgeModelEvent
a_km1_ch2_ansYes6_fuq4 =
  KnowledgeModelEvent
    { uuid = u' "c626fd42-80b8-4fd2-a16b-d38eeb8262f1"
    , parentUuid = q4_it1_q6_answerYes.uuid
    , entityUuid = q4_it1_q6_aYes_followUpQuestion4.uuid
    , content =
        AddQuestionEvent' $
          AddListQuestionEvent'
            AddListQuestionEvent
              { title = q4_it1_q6_aYes_followUpQuestion4.title
              , text = q4_it1_q6_aYes_followUpQuestion4.text
              , requiredPhaseUuid = q4_it1_q6_aYes_followUpQuestion4.requiredPhaseUuid
              , annotations = q4_it1_q6_aYes_followUpQuestion4.annotations
              , tagUuids = q4_it1_q6_aYes_followUpQuestion4.tagUuids
              }
    , createdAt = dt'' 2018 1 21 49
    }

a_km1_ch2_ansYes6_fuq5 :: KnowledgeModelEvent
a_km1_ch2_ansYes6_fuq5 =
  KnowledgeModelEvent
    { uuid = u' "11872ad2-0d3d-4ab6-b81c-17d234bab6ba"
    , parentUuid = q4_it1_q6_answerYes.uuid
    , entityUuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , content =
        AddQuestionEvent' $
          AddIntegrationQuestionEvent' $
            AddIntegrationQuestionEvent
              { title = q4_it1_q6_aYes_followUpQuestion5.title
              , text = q4_it1_q6_aYes_followUpQuestion5.text
              , requiredPhaseUuid = q4_it1_q6_aYes_followUpQuestion5.requiredPhaseUuid
              , annotations = q4_it1_q6_aYes_followUpQuestion5.annotations
              , tagUuids = q4_it1_q6_aYes_followUpQuestion5.tagUuids
              , integrationUuid = q4_it1_q6_aYes_followUpQuestion5.integrationUuid
              , variables = q4_it1_q6_aYes_followUpQuestion5.variables
              }
    , createdAt = dt'' 2018 1 21 50
    }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: KnowledgeModelEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  KnowledgeModelEvent
    { uuid = u' "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , parentUuid = q2_aYes_fuq1_answerYes.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , content =
        EditQuestionEvent' $
          EditOptionsQuestionEvent' $
            EditOptionsQuestionEvent
              { title = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.title
              , text = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.text
              , requiredPhaseUuid = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.requiredPhaseUuid
              , annotations = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.annotations
              , tagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.tagUuids
              , expertUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.expertUuids
              , referenceUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.referenceUuids
              , answerUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.answerUuids
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 :: KnowledgeModelEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 =
  KnowledgeModelEvent
    { uuid = u' "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , parentUuid = q2_aYes_fuq1_answerYes.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , content = EditQuestionEvent' $ EditOptionsQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2__content
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2__content :: EditOptionsQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2__content =
  EditOptionsQuestionEvent
    { title = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.title
    , text = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.text
    , requiredPhaseUuid = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.requiredPhaseUuid
    , annotations = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.annotations
    , tagUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2Edited.tagUuids
    , expertUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2.expertUuids
    , referenceUuids = ChangedValue $ q2_aYes_fuq1_aYes_fuQuestion2.referenceUuids
    , answerUuids = ChangedValue [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid, q2_aYes_fuq1_aYes_fuq2_answerNo.uuid]
    }

e_km1_ch2_ansMaybe6_fuq4 :: KnowledgeModelEvent
e_km1_ch2_ansMaybe6_fuq4 =
  KnowledgeModelEvent
    { uuid = u' "378f1fb0-e714-400b-a23d-fa939acd3f45"
    , parentUuid = q4_it1_q6_answerNo.uuid
    , entityUuid = q4_it1_q6_aYes_followUpQuestion4Edited.uuid
    , content = EditQuestionEvent' $ EditListQuestionEvent' e_km1_ch2_ansMaybe6_fuq4__content
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch2_ansMaybe6_fuq4__content :: EditListQuestionEvent
e_km1_ch2_ansMaybe6_fuq4__content =
  EditListQuestionEvent
    { title = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.title
    , text = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.text
    , requiredPhaseUuid = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.requiredPhaseUuid
    , annotations = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.annotations
    , tagUuids = ChangedValue $ q4_it1_q6_aYes_followUpQuestion4Edited.tagUuids
    , expertUuids = NothingChanged
    , referenceUuids = NothingChanged
    , itemTemplateQuestionUuids = ChangedValue [q4_it1_q6_aYes_fuq4_it_question2.uuid, q4_it1_q6_aYes_fuq4_it_question1.uuid]
    }

d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: KnowledgeModelEvent
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  KnowledgeModelEvent
    { uuid = u' "db69d694-cfb6-4461-8a13-81c01638f348"
    , parentUuid = q2_aYes_fuq1_answerYes.uuid
    , entityUuid = q2_aYes_fuq1_aYes_fuQuestion2.uuid
    , content = DeleteQuestionEvent' DeleteQuestionEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch3_q11_cho1 :: KnowledgeModelEvent
a_km1_ch3_q11_cho1 =
  KnowledgeModelEvent
    { uuid = u' "0a58e6bf-a185-400f-945a-17a96fac6073"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice1.uuid
    , content =
        AddChoiceEvent' $
          AddChoiceEvent
            { aLabel = q11_choice1.aLabel
            , annotations = q11_choice1.annotations
            }
    , createdAt = dt'' 2018 1 21 61
    }

a_km1_ch3_q11_cho2 :: KnowledgeModelEvent
a_km1_ch3_q11_cho2 =
  KnowledgeModelEvent
    { uuid = u' "da967bd5-4eb3-4329-ad79-63f49ad361c3"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice2.uuid
    , content =
        AddChoiceEvent' $
          AddChoiceEvent
            { aLabel = q11_choice2.aLabel
            , annotations = q11_choice2.annotations
            }
    , createdAt = dt'' 2018 1 21 62
    }

a_km1_ch3_q11_cho3 :: KnowledgeModelEvent
a_km1_ch3_q11_cho3 =
  KnowledgeModelEvent
    { uuid = u' "1c8561ae-44fb-4e5e-96e7-2582563330de"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice3.uuid
    , content =
        AddChoiceEvent' $
          AddChoiceEvent
            { aLabel = q11_choice3.aLabel
            , annotations = q11_choice3.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch3_q11_cho1 :: KnowledgeModelEvent
e_km1_ch3_q11_cho1 =
  KnowledgeModelEvent
    { uuid = u' "bda5b518-f7f0-4ea3-b609-9117f5931c54"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice1Edited.uuid
    , content =
        EditChoiceEvent' $
          EditChoiceEvent
            { aLabel = ChangedValue $ q11_choice1Edited.aLabel
            , annotations = ChangedValue $ q11_choice1Edited.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_ch3_q11_cho1 :: KnowledgeModelEvent
d_km1_ch3_q11_cho1 =
  KnowledgeModelEvent
    { uuid = u' "9f877d39-103c-494a-b863-19050029242c"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice1.uuid
    , content = DeleteChoiceEvent' DeleteChoiceEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_eAlbert :: KnowledgeModelEvent
a_km1_ch1_q2_eAlbert =
  KnowledgeModelEvent
    { uuid = u' "ec76054f-d059-4a5f-81c9-1817004a913c"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eAlbert.uuid
    , content =
        AddExpertEvent' $
          AddExpertEvent
            { name = km1_ch1_q2_eAlbert.name
            , email = km1_ch1_q2_eAlbert.email
            , annotations = km1_ch1_q2_eAlbert.annotations
            }
    , createdAt = dt'' 2018 1 21 34
    }

a_km1_ch2_q6_eAlbert :: KnowledgeModelEvent
a_km1_ch2_q6_eAlbert =
  KnowledgeModelEvent
    { uuid = u' "eb6bb073-ecba-4cd0-91a3-ff31d374601f"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = km1_ch2_q6_eAlbert.uuid
    , content =
        AddExpertEvent' $
          AddExpertEvent
            { name = km1_ch2_q6_eAlbert.name
            , email = km1_ch2_q6_eAlbert.email
            , annotations = km1_ch2_q6_eAlbert.annotations
            }
    , createdAt = dt'' 2018 1 21 53
    }

a_km1_ch1_q2_eNikola :: KnowledgeModelEvent
a_km1_ch1_q2_eNikola =
  KnowledgeModelEvent
    { uuid = u' "40bb45bd-4195-4430-ac8f-16ac5a61ece0"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eNikola.uuid
    , content =
        AddExpertEvent' $
          AddExpertEvent
            { name = km1_ch1_q2_eNikola.name
            , email = km1_ch1_q2_eNikola.email
            , annotations = km1_ch1_q2_eNikola.annotations
            }
    , createdAt = dt'' 2018 1 21 35
    }

a_km1_ch2_q6_eNikola :: KnowledgeModelEvent
a_km1_ch2_q6_eNikola =
  KnowledgeModelEvent
    { uuid = u' "53653d05-6d5a-4b76-bbc6-15ca8314ad69"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = km1_ch2_q6_eNikola.uuid
    , content =
        AddExpertEvent' $
          AddExpertEvent
            { name = km1_ch2_q6_eNikola.name
            , email = km1_ch2_q6_eNikola.email
            , annotations = km1_ch2_q6_eNikola.annotations
            }
    , createdAt = dt'' 2018 1 21 54
    }

a_km1_ch1_q2_eIsaac :: KnowledgeModelEvent
a_km1_ch1_q2_eIsaac =
  KnowledgeModelEvent
    { uuid = u' "2d5eedae-1782-44ac-9d4e-3db769161448"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eIsaac.uuid
    , content =
        AddExpertEvent' $
          AddExpertEvent
            { name = km1_ch1_q2_eIsaac.name
            , email = km1_ch1_q2_eIsaac.email
            , annotations = km1_ch1_q2_eIsaac.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_eAlbert :: KnowledgeModelEvent
e_km1_ch1_q2_eAlbert =
  KnowledgeModelEvent
    { uuid = u' "01686131-2423-4d97-a949-4fea2c9ce3b7"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eAlbertEdited.uuid
    , content =
        EditExpertEvent' $
          EditExpertEvent
            { name = ChangedValue $ km1_ch1_q2_eAlbertEdited.name
            , email = ChangedValue $ km1_ch1_q2_eAlbertEdited.email
            , annotations = ChangedValue $ km1_ch1_q2_eAlbertEdited.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_ch1_q2_eNikola :: KnowledgeModelEvent
d_km1_ch1_q2_eNikola =
  KnowledgeModelEvent
    { uuid = u' "f20bc988-6d44-4051-990d-d16b24f369ac"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eNikola.uuid
    , content = DeleteExpertEvent' DeleteExpertEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_rCh1 :: KnowledgeModelEvent
a_km1_ch1_q2_rCh1 =
  KnowledgeModelEvent
    { uuid = u' "1177d72f-b7d8-466d-ad33-d5f82d0f192a"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r1.uuid
    , content =
        AddReferenceEvent' $
          AddResourcePageReferenceEvent' $
            AddResourcePageReferenceEvent
              { resourcePageUuid = km1_ch2_q6_r1.resourcePageUuid
              , annotations = km1_ch1_q2_r1.annotations
              }
    , createdAt = dt'' 2018 1 21 36
    }

a_km1_ch2_q6_rCh1 :: KnowledgeModelEvent
a_km1_ch2_q6_rCh1 =
  KnowledgeModelEvent
    { uuid = u' "a3f6ee9a-803f-4911-9566-734a6358913a"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = km1_ch2_q6_r1.uuid
    , content =
        AddReferenceEvent' $
          AddResourcePageReferenceEvent' $
            AddResourcePageReferenceEvent
              { resourcePageUuid = km1_ch2_q6_r1.resourcePageUuid
              , annotations = km1_ch2_q6_r1.annotations
              }
    , createdAt = dt'' 2018 1 21 55
    }

a_km1_ch1_q2_rCh2 :: KnowledgeModelEvent
a_km1_ch1_q2_rCh2 =
  KnowledgeModelEvent
    { uuid = u' "4814f50f-8838-4b53-8b18-c0f8c568220e"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r2.uuid
    , content =
        AddReferenceEvent' $
          AddURLReferenceEvent' $
            AddURLReferenceEvent
              { url = km1_ch1_q2_r2.url
              , aLabel = km1_ch1_q2_r2.aLabel
              , annotations = km1_ch1_q2_r2.annotations
              }
    , createdAt = dt'' 2018 1 21 37
    }

a_km1_ch2_q6_rCh2 :: KnowledgeModelEvent
a_km1_ch2_q6_rCh2 =
  KnowledgeModelEvent
    { uuid = u' "a4ae3400-dd3c-41ab-b796-4bf9d0bdafe7"
    , parentUuid = q4_it1_question6.uuid
    , entityUuid = km1_ch2_q6_r2.uuid
    , content =
        AddReferenceEvent' $
          AddURLReferenceEvent' $
            AddURLReferenceEvent
              { url = km1_ch2_q6_r2.url
              , aLabel = km1_ch2_q6_r2.aLabel
              , annotations = km1_ch2_q6_r2.annotations
              }
    , createdAt = dt'' 2018 1 21 56
    }

a_km1_ch1_q2_rCh3 :: KnowledgeModelEvent
a_km1_ch1_q2_rCh3 =
  KnowledgeModelEvent
    { uuid = u' "45d8ec86-34bc-4e8f-b42a-48a567a77d8b"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r3.uuid
    , content =
        AddReferenceEvent' $
          AddCrossReferenceEvent' $
            AddCrossReferenceEvent
              { targetUuid = km1_ch1_q2_r3.targetUuid
              , description = km1_ch1_q2_r3.description
              , annotations = km1_ch1_q2_r3.annotations
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_rCh1 :: KnowledgeModelEvent
e_km1_ch1_q2_rCh1 =
  KnowledgeModelEvent
    { uuid = u' "08cd9afc-d416-48ab-8669-17e87ceb15dc"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r1Edited.uuid
    , content =
        EditReferenceEvent' $
          EditResourcePageReferenceEvent' $
            EditResourcePageReferenceEvent
              { resourcePageUuid = ChangedValue $ km1_ch1_q2_r1Edited.resourcePageUuid
              , annotations = ChangedValue $ km1_ch1_q2_r1Edited.annotations
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_rCh1_type :: KnowledgeModelEvent
e_km1_ch1_q2_rCh1_type =
  KnowledgeModelEvent
    { uuid = u' "4e1058cf-9044-42a0-901c-816bd6847b17"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r1WithNewType.uuid
    , content =
        EditReferenceEvent' $
          EditURLReferenceEvent' $
            EditURLReferenceEvent
              { url = ChangedValue $ km1_ch1_q2_r1WithNewType.url
              , aLabel = ChangedValue $ km1_ch1_q2_r1WithNewType.aLabel
              , annotations = ChangedValue $ km1_ch1_q2_r1WithNewType.annotations
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_rCh2 :: KnowledgeModelEvent
e_km1_ch1_q2_rCh2 =
  KnowledgeModelEvent
    { uuid = u' "f96588ae-1657-406e-9810-1d00f5e24a96"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r2Edited.uuid
    , content =
        EditReferenceEvent' $
          EditURLReferenceEvent' $
            EditURLReferenceEvent
              { url = ChangedValue $ km1_ch1_q2_r2Edited.url
              , aLabel = ChangedValue $ km1_ch1_q2_r2Edited.aLabel
              , annotations = ChangedValue $ km1_ch1_q2_r2Edited.annotations
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_rCh2_type :: KnowledgeModelEvent
e_km1_ch1_q2_rCh2_type =
  KnowledgeModelEvent
    { uuid = u' "e0a19e9d-fb36-47b3-bc23-f752f7403937"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r2WithNewType.uuid
    , content =
        EditReferenceEvent' $
          EditCrossReferenceEvent' $
            EditCrossReferenceEvent
              { targetUuid = ChangedValue $ km1_ch1_q2_r2WithNewType.targetUuid
              , description = ChangedValue $ km1_ch1_q2_r2WithNewType.description
              , annotations = ChangedValue $ km1_ch1_q2_r2WithNewType.annotations
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_rCh3 :: KnowledgeModelEvent
e_km1_ch1_q2_rCh3 =
  KnowledgeModelEvent
    { uuid = u' "d3a7b6a6-9e87-4308-a103-88245537c26e"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r3Edited.uuid
    , content =
        EditReferenceEvent' $
          EditCrossReferenceEvent' $
            EditCrossReferenceEvent
              { targetUuid = ChangedValue $ km1_ch1_q2_r3Edited.targetUuid
              , description = ChangedValue $ km1_ch1_q2_r3Edited.description
              , annotations = ChangedValue $ km1_ch1_q2_r3Edited.annotations
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_ch1_q2_rCh3_type :: KnowledgeModelEvent
e_km1_ch1_q2_rCh3_type =
  KnowledgeModelEvent
    { uuid = u' "f8528e3b-4904-4ad8-87b8-809d7e40c087"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r3WithNewType.uuid
    , content =
        EditReferenceEvent' $
          EditResourcePageReferenceEvent' $
            EditResourcePageReferenceEvent
              { resourcePageUuid = ChangedValue $ km1_ch1_q2_r3WithNewType.resourcePageUuid
              , annotations = ChangedValue $ km1_ch1_q2_r3WithNewType.annotations
              }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_ch1_q2_rCh2 :: KnowledgeModelEvent
d_km1_ch1_q2_rCh2 =
  KnowledgeModelEvent
    { uuid = u' "3cc15f31-4801-404f-ba48-6b91f77d1abe"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r2.uuid
    , content = DeleteReferenceEvent' DeleteReferenceEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_tds :: KnowledgeModelEvent
a_km1_tds =
  KnowledgeModelEvent
    { uuid = u' "dedc4a9d-00d9-41b6-8494-a10a238be03b"
    , parentUuid = km1.uuid
    , entityUuid = tagDataScience.uuid
    , content =
        AddTagEvent' $
          AddTagEvent
            { name = tagDataScience.name
            , description = tagDataScience.description
            , color = tagDataScience.color
            , annotations = tagDataScience.annotations
            }
    , createdAt = dt'' 2018 1 21 12
    }

a_km1_tbi :: KnowledgeModelEvent
a_km1_tbi =
  KnowledgeModelEvent
    { uuid = u' "b6b0e53c-5702-403c-950c-e04960e09e73"
    , parentUuid = km1.uuid
    , entityUuid = tagBioinformatics.uuid
    , content =
        AddTagEvent' $
          AddTagEvent
            { name = tagBioinformatics.name
            , description = tagBioinformatics.description
            , color = tagBioinformatics.color
            , annotations = tagBioinformatics.annotations
            }
    , createdAt = dt'' 2018 1 21 13
    }

e_km1_tds :: KnowledgeModelEvent
e_km1_tds =
  KnowledgeModelEvent
    { uuid = u' "f68f764b-48d1-4b30-8d53-48cfa2752801"
    , parentUuid = km1.uuid
    , entityUuid = tagDataScienceEdited.uuid
    , content =
        EditTagEvent' $
          EditTagEvent
            { name = ChangedValue $ tagDataScienceEdited.name
            , description = ChangedValue $ tagDataScienceEdited.description
            , color = ChangedValue $ tagDataScienceEdited.color
            , annotations = ChangedValue $ tagDataScienceEdited.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_tds :: KnowledgeModelEvent
d_km1_tds =
  KnowledgeModelEvent
    { uuid = u' "969d00c2-062d-4763-a372-536d486c532f"
    , parentUuid = km1.uuid
    , entityUuid = tagDataScience.uuid
    , content = DeleteTagEvent' DeleteTagEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ir :: KnowledgeModelEvent
a_km1_ir =
  KnowledgeModelEvent
    { uuid = u' "beffa456-eecc-4cd3-bc93-07cad5f87ec9"
    , parentUuid = km1.uuid
    , entityUuid = repositoryApi.uuid
    , content =
        AddIntegrationEvent' $
          AddApiIntegrationEvent' $
            AddApiIntegrationEvent
              { name = repositoryApi.name
              , variables = repositoryApi.variables
              , allowCustomReply = repositoryApi.allowCustomReply
              , requestMethod = repositoryApi.requestMethod
              , requestUrl = repositoryApi.requestUrl
              , requestHeaders = repositoryApi.requestHeaders
              , requestBody = repositoryApi.requestBody
              , requestAllowEmptySearch = repositoryApi.requestAllowEmptySearch
              , responseListField = repositoryApi.responseListField
              , responseItemTemplate = repositoryApi.responseItemTemplate
              , responseItemTemplateForSelection = repositoryApi.responseItemTemplateForSelection
              , testQ = repositoryApi.testQ
              , testVariables = repositoryApi.testVariables
              , testResponse = repositoryApi.testResponse
              , annotations = repositoryApi.annotations
              }
    , createdAt = dt'' 2018 1 21 14
    }

a_km1_iop :: KnowledgeModelEvent
a_km1_iop =
  KnowledgeModelEvent
    { uuid = u' "3f94cb01-6f92-4eb6-975b-385c02b831bc"
    , parentUuid = km1.uuid
    , entityUuid = ontologyPortal.uuid
    , content =
        AddIntegrationEvent' $
          AddApiLegacyIntegrationEvent' $
            AddApiLegacyIntegrationEvent
              { iId = ontologyPortal.iId
              , name = ontologyPortal.name
              , variables = ontologyPortal.variables
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
              }
    , createdAt = dt'' 2018 1 21 15
    }

a_km1_ibp :: KnowledgeModelEvent
a_km1_ibp =
  KnowledgeModelEvent
    { uuid = u' "5c47b31c-84d0-4792-99ce-09154642105d"
    , parentUuid = km1.uuid
    , entityUuid = bioPortal.uuid
    , content =
        AddIntegrationEvent' $
          AddApiLegacyIntegrationEvent' $
            AddApiLegacyIntegrationEvent
              { iId = bioPortal.iId
              , name = bioPortal.name
              , variables = bioPortal.variables
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
              }
    , createdAt = dt'' 2018 1 21 16
    }

a_km1_iwp :: KnowledgeModelEvent
a_km1_iwp =
  KnowledgeModelEvent
    { uuid = u' "cd3275d5-1c51-4609-bf6e-3bf1b2070dd5"
    , parentUuid = km1.uuid
    , entityUuid = widgetPortal.uuid
    , content =
        AddIntegrationEvent' $
          AddWidgetIntegrationEvent' $
            AddWidgetIntegrationEvent
              { iId = widgetPortal.iId
              , name = widgetPortal.name
              , variables = widgetPortal.variables
              , logo = widgetPortal.logo
              , widgetUrl = widgetPortal.widgetUrl
              , itemUrl = widgetPortal.itemUrl
              , annotations = widgetPortal.annotations
              }
    , createdAt = dt'' 2018 1 21 17
    }

e_km1_ir :: KnowledgeModelEvent
e_km1_ir =
  KnowledgeModelEvent
    { uuid = u' "f26be960-5705-4bc6-8348-bf8719a40eab"
    , parentUuid = km1.uuid
    , entityUuid = repositoryApi.uuid
    , content =
        EditIntegrationEvent' $
          EditApiIntegrationEvent' $
            EditApiIntegrationEvent
              { name = ChangedValue repositoryApi.name
              , variables = ChangedValue repositoryApi.variables
              , allowCustomReply = ChangedValue repositoryApi.allowCustomReply
              , requestMethod = ChangedValue repositoryApi.requestMethod
              , requestUrl = ChangedValue repositoryApi.requestUrl
              , requestHeaders = ChangedValue repositoryApi.requestHeaders
              , requestBody = ChangedValue repositoryApi.requestBody
              , requestAllowEmptySearch = ChangedValue repositoryApi.requestAllowEmptySearch
              , responseListField = ChangedValue repositoryApi.responseListField
              , responseItemTemplate = ChangedValue repositoryApi.responseItemTemplate
              , responseItemTemplateForSelection = ChangedValue repositoryApi.responseItemTemplateForSelection
              , testQ = ChangedValue repositoryApi.testQ
              , testVariables = ChangedValue repositoryApi.testVariables
              , testResponse = ChangedValue repositoryApi.testResponse
              , annotations = ChangedValue repositoryApi.annotations
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_iop :: KnowledgeModelEvent
e_km1_iop =
  KnowledgeModelEvent
    { uuid = u' "3456a254-c5bc-4c0e-8ff9-f5e080765a71"
    , parentUuid = km1.uuid
    , entityUuid = ontologyPortalEdited.uuid
    , content =
        EditIntegrationEvent' $
          EditApiLegacyIntegrationEvent' $
            EditApiLegacyIntegrationEvent
              { iId = ChangedValue $ ontologyPortalEdited.iId
              , name = ChangedValue $ ontologyPortalEdited.name
              , variables = ChangedValue $ ontologyPortalEdited.variables
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
              }
    , createdAt = dt'' 2018 1 21 0
    }

e_km1_iwp :: KnowledgeModelEvent
e_km1_iwp =
  KnowledgeModelEvent
    { uuid = u' "2c62e0d2-7e5f-4acb-9b8e-826202fc4fa9"
    , parentUuid = km1.uuid
    , entityUuid = widgetPortalEdited.uuid
    , content =
        EditIntegrationEvent' $
          EditWidgetIntegrationEvent' $
            EditWidgetIntegrationEvent
              { iId = ChangedValue $ widgetPortalEdited.iId
              , name = ChangedValue $ widgetPortalEdited.name
              , variables = ChangedValue $ widgetPortalEdited.variables
              , logo = ChangedValue $ widgetPortalEdited.logo
              , widgetUrl = ChangedValue $ widgetPortalEdited.widgetUrl
              , itemUrl = ChangedValue $ widgetPortalEdited.itemUrl
              , annotations = ChangedValue $ widgetPortalEdited.annotations
              }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_iop :: KnowledgeModelEvent
d_km1_iop =
  KnowledgeModelEvent
    { uuid = u' "d211d46f-5358-497a-92a0-e0bde08ce3d3"
    , parentUuid = km1.uuid
    , entityUuid = ontologyPortal.uuid
    , content = DeleteIntegrationEvent' DeleteIntegrationEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_mtrF :: KnowledgeModelEvent
a_km1_mtrF =
  KnowledgeModelEvent
    { uuid = u' "d22017a1-89ea-4aba-b2df-92ea2cf4eac5"
    , parentUuid = km1.uuid
    , entityUuid = metricF.uuid
    , content =
        AddMetricEvent' $
          AddMetricEvent
            { title = metricF.title
            , abbreviation = metricF.abbreviation
            , description = metricF.description
            , annotations = metricF.annotations
            }
    , createdAt = dt'' 2018 1 21 3
    }

a_km1_mtrA :: KnowledgeModelEvent
a_km1_mtrA =
  KnowledgeModelEvent
    { uuid = u' "d7d4052e-5413-48ec-8e0e-0b43e027369e"
    , parentUuid = km1.uuid
    , entityUuid = metricA.uuid
    , content =
        AddMetricEvent' $
          AddMetricEvent
            { title = metricA.title
            , abbreviation = metricA.abbreviation
            , description = metricA.description
            , annotations = metricA.annotations
            }
    , createdAt = dt'' 2018 1 21 4
    }

a_km1_mtrI :: KnowledgeModelEvent
a_km1_mtrI =
  KnowledgeModelEvent
    { uuid = u' "6b6e0cb2-5f1d-42ed-9576-c454664a7884"
    , parentUuid = km1.uuid
    , entityUuid = metricI.uuid
    , content =
        AddMetricEvent' $
          AddMetricEvent
            { title = metricI.title
            , abbreviation = metricI.abbreviation
            , description = metricI.description
            , annotations = metricI.annotations
            }
    , createdAt = dt'' 2018 1 21 5
    }

a_km1_mtrR :: KnowledgeModelEvent
a_km1_mtrR =
  KnowledgeModelEvent
    { uuid = u' "6d62e9fe-0a67-4f63-8ff8-4553f1154018"
    , parentUuid = km1.uuid
    , entityUuid = metricR.uuid
    , content =
        AddMetricEvent' $
          AddMetricEvent
            { title = metricR.title
            , abbreviation = metricR.abbreviation
            , description = metricR.description
            , annotations = metricR.annotations
            }
    , createdAt = dt'' 2018 1 21 6
    }

a_km1_mtrG :: KnowledgeModelEvent
a_km1_mtrG =
  KnowledgeModelEvent
    { uuid = u' "84fa1ecf-a445-4a54-a1d5-34062ddc7735"
    , parentUuid = km1.uuid
    , entityUuid = metricG.uuid
    , content =
        AddMetricEvent' $
          AddMetricEvent
            { title = metricG.title
            , abbreviation = metricG.abbreviation
            , description = metricG.description
            , annotations = metricG.annotations
            }
    , createdAt = dt'' 2018 1 21 7
    }

a_km1_mtrO :: KnowledgeModelEvent
a_km1_mtrO =
  KnowledgeModelEvent
    { uuid = u' "c7b2f5a9-1b18-44ea-9296-259335e410f5"
    , parentUuid = km1.uuid
    , entityUuid = metricO.uuid
    , content =
        AddMetricEvent' $
          AddMetricEvent
            { title = metricO.title
            , abbreviation = metricO.abbreviation
            , description = metricO.description
            , annotations = metricO.annotations
            }
    , createdAt = dt'' 2018 1 21 8
    }

e_km1_mtrF :: KnowledgeModelEvent
e_km1_mtrF =
  KnowledgeModelEvent
    { uuid = u' "da2350c5-b881-4e46-a8b1-94d476d1fc74"
    , parentUuid = km1.uuid
    , entityUuid = metricFEdited.uuid
    , content =
        EditMetricEvent' $
          EditMetricEvent
            { title = ChangedValue $ metricFEdited.title
            , abbreviation = ChangedValue $ metricFEdited.abbreviation
            , description = ChangedValue $ metricFEdited.description
            , annotations = ChangedValue $ metricFEdited.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_mtrF :: KnowledgeModelEvent
d_km1_mtrF =
  KnowledgeModelEvent
    { uuid = u' "e1b1a8ed-f23d-49aa-80a9-2077055aac87"
    , parentUuid = km1.uuid
    , entityUuid = metricF.uuid
    , content = DeleteMetricEvent' DeleteMetricEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_phs1 :: KnowledgeModelEvent
a_km1_phs1 =
  KnowledgeModelEvent
    { uuid = u' "e3ba08a4-1775-4a74-b062-625c18afa65f"
    , parentUuid = km1.uuid
    , entityUuid = phase1.uuid
    , content =
        AddPhaseEvent' $
          AddPhaseEvent
            { title = phase1.title
            , description = phase1.description
            , annotations = phase1.annotations
            }
    , createdAt = dt'' 2018 1 21 9
    }

a_km1_phs2 :: KnowledgeModelEvent
a_km1_phs2 =
  KnowledgeModelEvent
    { uuid = u' "4853d211-17fd-46fa-8327-d45a58a6eb12"
    , parentUuid = km1.uuid
    , entityUuid = phase2.uuid
    , content =
        AddPhaseEvent' $
          AddPhaseEvent
            { title = phase2.title
            , description = phase2.description
            , annotations = phase2.annotations
            }
    , createdAt = dt'' 2018 1 21 10
    }

a_km1_phs3 :: KnowledgeModelEvent
a_km1_phs3 =
  KnowledgeModelEvent
    { uuid = u' "e1c813ec-1ee2-46be-bc85-4386aef91657"
    , parentUuid = km1.uuid
    , entityUuid = phase3.uuid
    , content =
        AddPhaseEvent' $
          AddPhaseEvent
            { title = phase3.title
            , description = phase3.description
            , annotations = phase3.annotations
            }
    , createdAt = dt'' 2018 1 21 11
    }

e_km1_phs1 :: KnowledgeModelEvent
e_km1_phs1 =
  KnowledgeModelEvent
    { uuid = u' "d7e65e08-52bc-4096-a24c-1dc737e64266"
    , parentUuid = km1.uuid
    , entityUuid = phase1Edited.uuid
    , content =
        EditPhaseEvent' $
          EditPhaseEvent
            { title = ChangedValue $ phase1Edited.title
            , description = ChangedValue $ phase1Edited.description
            , annotations = ChangedValue $ phase1Edited.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_phs1 :: KnowledgeModelEvent
d_km1_phs1 =
  KnowledgeModelEvent
    { uuid = u' "18ea7949-7e5a-4fae-8f5f-67b509ae397a"
    , parentUuid = km1.uuid
    , entityUuid = phase1.uuid
    , content = DeletePhaseEvent' DeletePhaseEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_rc1 :: KnowledgeModelEvent
a_km1_rc1 =
  KnowledgeModelEvent
    { uuid = u' "311d5db4-c075-4e28-97d1-b8a1501e062e"
    , parentUuid = km1.uuid
    , entityUuid = rc1.uuid
    , content =
        AddResourceCollectionEvent' $
          AddResourceCollectionEvent
            { title = rc1.title
            , annotations = rc1.annotations
            }
    , createdAt = dt'' 2018 1 21 18
    }

a_km1_rc2 :: KnowledgeModelEvent
a_km1_rc2 =
  KnowledgeModelEvent
    { uuid = u' "65d02da0-e76e-47af-82e3-dc40c7fef880"
    , parentUuid = km1.uuid
    , entityUuid = rc2.uuid
    , content =
        AddResourceCollectionEvent' $
          AddResourceCollectionEvent
            { title = rc2.title
            , annotations = rc2.annotations
            }
    , createdAt = dt'' 2018 1 21 21
    }

e_km1_rc1 :: KnowledgeModelEvent
e_km1_rc1 =
  KnowledgeModelEvent
    { uuid = u' "721f3fc9-8d2c-4c89-b4d1-e013c1e58a36"
    , parentUuid = km1.uuid
    , entityUuid = rc1Edited.uuid
    , content =
        EditResourceCollectionEvent' $
          EditResourceCollectionEvent
            { title = ChangedValue rc1Edited.title
            , resourcePageUuids = ChangedValue rc1Edited.resourcePageUuids
            , annotations = ChangedValue rc1Edited.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_rc1 :: KnowledgeModelEvent
d_km1_rc1 =
  KnowledgeModelEvent
    { uuid = u' "2ca967e3-4a3b-4c2a-9ad8-11177c00f457"
    , parentUuid = km1.uuid
    , entityUuid = rc1.uuid
    , content = DeleteResourceCollectionEvent' DeleteResourceCollectionEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
a_km1_rc1_rp1 :: KnowledgeModelEvent
a_km1_rc1_rp1 =
  KnowledgeModelEvent
    { uuid = u' "7be50408-2752-46fb-803d-81b136be1021"
    , parentUuid = rc1.uuid
    , entityUuid = rc1_rp1.uuid
    , content =
        AddResourcePageEvent' $
          AddResourcePageEvent
            { title = rc1_rp1.title
            , content = rc1_rp1.content
            , annotations = rc1_rp1.annotations
            }
    , createdAt = dt'' 2018 1 21 19
    }

a_km1_rc1_rp2 :: KnowledgeModelEvent
a_km1_rc1_rp2 =
  KnowledgeModelEvent
    { uuid = u' "317a09b4-acb7-47e9-bf7f-c48535a3209c"
    , parentUuid = rc1.uuid
    , entityUuid = rc1_rp2.uuid
    , content =
        AddResourcePageEvent' $
          AddResourcePageEvent
            { title = rc1_rp2.title
            , content = rc1_rp2.content
            , annotations = rc1_rp2.annotations
            }
    , createdAt = dt'' 2018 1 21 20
    }

a_km1_rc2_rp1 :: KnowledgeModelEvent
a_km1_rc2_rp1 =
  KnowledgeModelEvent
    { uuid = u' "0fc1cd12-d9a6-4ec7-9fd7-8e8babb2e229"
    , parentUuid = rc2.uuid
    , entityUuid = rc2_rp1.uuid
    , content =
        AddResourcePageEvent' $
          AddResourcePageEvent
            { title = rc2_rp1.title
            , content = rc2_rp1.content
            , annotations = rc2_rp1.annotations
            }
    , createdAt = dt'' 2018 1 21 22
    }

e_km1_rc1_rp1 :: KnowledgeModelEvent
e_km1_rc1_rp1 =
  KnowledgeModelEvent
    { uuid = u' "e9195024-2978-4edd-8f2c-f1031096b064"
    , parentUuid = rc1.uuid
    , entityUuid = rc1_rp1Edited.uuid
    , content =
        EditResourcePageEvent' $
          EditResourcePageEvent
            { title = ChangedValue rc1_rp1Edited.title
            , content = ChangedValue rc1_rp1Edited.content
            , annotations = ChangedValue rc1_rp1Edited.annotations
            }
    , createdAt = dt'' 2018 1 21 0
    }

d_km1_rc1_rp1 :: KnowledgeModelEvent
d_km1_rc1_rp1 =
  KnowledgeModelEvent
    { uuid = u' "185c6c76-c21d-4840-b48e-2ff454ad3b76"
    , parentUuid = rc1.uuid
    , entityUuid = rc1_rp1.uuid
    , content = DeleteResourcePageEvent' DeleteResourcePageEvent
    , createdAt = dt'' 2018 1 21 0
    }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
m_km1_ch1_q1__to_ch2 :: KnowledgeModelEvent
m_km1_ch1_q1__to_ch2 =
  KnowledgeModelEvent
    { uuid = u' "f13a1d1b-5cb6-458a-ad99-cafe3912aa1d"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , content =
        MoveQuestionEvent' $
          MoveQuestionEvent
            { targetUuid = chapter2.uuid
            }
    , createdAt = dt'' 2018 1 21 0
    }

m_km1_ch1_q1__to_ch2_q3_aNo :: KnowledgeModelEvent
m_km1_ch1_q1__to_ch2_q3_aNo =
  KnowledgeModelEvent
    { uuid = u' "3bf501a1-cbc2-4b94-9b17-d23f0bad7fc9"
    , parentUuid = chapter1.uuid
    , entityUuid = question1.uuid
    , content =
        MoveQuestionEvent' $
          MoveQuestionEvent
            { targetUuid = q3_answerNo.uuid
            }
    , createdAt = dt'' 2018 1 21 0
    }

m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo :: KnowledgeModelEvent
m_km1_ch2_q4_it1_q5__to_ch2_q4_it1_q6_aNo =
  KnowledgeModelEvent
    { uuid = u' "a2f35e98-dd67-45cf-a18e-a8a38382c7be"
    , parentUuid = question4.uuid
    , entityUuid = q4_it1_question5.uuid
    , content =
        MoveQuestionEvent' $
          MoveQuestionEvent
            { targetUuid = q4_it1_q6_answerNo.uuid
            }
    , createdAt = dt'' 2018 1 21 0
    }

m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4 :: KnowledgeModelEvent
m_km1_ch2_q4_it1_q6_aYes_fuq4_it_q1__to_ch2_q4 =
  KnowledgeModelEvent
    { uuid = u' "a2f35e98-dd67-45cf-a18e-a8a38382c7be"
    , parentUuid = q4_it1_q6_aYes_followUpQuestion4.uuid
    , entityUuid = q4_it1_q6_aYes_fuq4_it_question1.uuid
    , content =
        MoveQuestionEvent' $
          MoveQuestionEvent
            { targetUuid = question4.uuid
            }
    , createdAt = dt'' 2018 1 21 0
    }

m_km1_ch1_q2_aYes__to_ch2_q3 :: KnowledgeModelEvent
m_km1_ch1_q2_aYes__to_ch2_q3 =
  KnowledgeModelEvent
    { uuid = u' "b660447a-ddbd-482a-9610-68dfca6a25fd"
    , parentUuid = question2.uuid
    , entityUuid = q2_answerYes.uuid
    , content =
        MoveAnswerEvent' $
          MoveAnswerEvent
            { targetUuid = question3.uuid
            }
    , createdAt = dt'' 2018 1 21 0
    }

m_km1_ch3_q11_cho1__to_ch3_q12 :: KnowledgeModelEvent
m_km1_ch3_q11_cho1__to_ch3_q12 =
  KnowledgeModelEvent
    { uuid = u' "0ffdff49-db85-4f28-b8a9-6b7a1569f5fd"
    , parentUuid = question11.uuid
    , entityUuid = q11_choice1.uuid
    , content =
        MoveChoiceEvent' $
          MoveChoiceEvent
            { targetUuid = question12.uuid
            }
    , createdAt = dt'' 2018 1 21 0
    }

m_km1_ch1_q2_eAlbert__to_ch2_q3 :: KnowledgeModelEvent
m_km1_ch1_q2_eAlbert__to_ch2_q3 =
  KnowledgeModelEvent
    { uuid = u' "35b18cb0-912f-4c76-9f80-b6bfc6479c7c"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_eAlbert.uuid
    , content =
        MoveExpertEvent' $
          MoveExpertEvent
            { targetUuid = question3.uuid
            }
    , createdAt = dt'' 2018 1 21 0
    }

m_km1_ch1_q2_r1__to_ch2_q3 :: KnowledgeModelEvent
m_km1_ch1_q2_r1__to_ch2_q3 =
  KnowledgeModelEvent
    { uuid = u' "1cc9ad2b-22bc-4806-902e-49b46ccc14d5"
    , parentUuid = question2.uuid
    , entityUuid = km1_ch1_q2_r1.uuid
    , content =
        MoveReferenceEvent' $
          MoveReferenceEvent
            { targetUuid = question3.uuid
            }
    , createdAt = dt'' 2018 1 21 0
    }
