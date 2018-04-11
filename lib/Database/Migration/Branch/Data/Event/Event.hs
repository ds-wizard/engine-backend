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
import Model.Event.Answer.AddAnswerEvent
import Model.Event.Answer.DeleteAnswerEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Chapter.DeleteChapterEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Event
import Model.Event.Expert.AddExpertEvent
import Model.Event.Expert.DeleteExpertEvent
import Model.Event.Expert.EditExpertEvent
import Model.Event.FollowUpQuestion.AddFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.DeleteFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.EditFollowUpQuestionEvent
import Model.Event.KnowledgeModel.AddKnowledgeModelEvent
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.Event.Question.AddQuestionEvent
import Model.Event.Question.DeleteQuestionEvent
import Model.Event.Question.EditQuestionEvent
import Model.Event.Reference.AddReferenceEvent
import Model.Event.Reference.DeleteReferenceEvent
import Model.Event.Reference.EditReferenceEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors

a_km1 :: AddKnowledgeModelEvent
a_km1 =
  AddKnowledgeModelEvent
  { _akmUuid = fromJust $ U.fromString "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f"
  , _akmKmUuid = km1WithoutChapters ^. uuid
  , _akmName = km1WithoutChapters ^. name
  }

e_km1 :: EditKnowledgeModelEvent
e_km1 =
  EditKnowledgeModelEvent
  { _ekmUuid = fromJust $ U.fromString "8294a55d-642d-416c-879b-5a42a4430c24"
  , _ekmKmUuid = km1 ^. uuid
  , _ekmName = Just $ km1WithChangeProperties ^. name
  , _ekmChapterIds = Just $ chapterIds km1WithChangeProperties
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1 :: AddChapterEvent
a_km1_ch1 =
  AddChapterEvent
  { _achUuid = fromJust $ U.fromString "dedc4a9d-00d9-41b6-8494-a10a238be03b"
  , _achKmUuid = km1 ^. uuid
  , _achChapterUuid = chapter1WithoutQuestions ^. uuid
  , _achTitle = chapter1WithoutQuestions ^. title
  , _achText = chapter1WithoutQuestions ^. text
  }

a_km1_ch2 :: AddChapterEvent
a_km1_ch2 =
  AddChapterEvent
  { _achUuid = fromJust $ U.fromString "6c4bba6e-864b-4871-98ca-49ac7a3e5eb5"
  , _achKmUuid = km1 ^. uuid
  , _achChapterUuid = chapter2WithoutQuestions ^. uuid
  , _achTitle = chapter2WithoutQuestions ^. title
  , _achText = chapter2WithoutQuestions ^. text
  }

a_km1_ch3 :: AddChapterEvent
a_km1_ch3 =
  AddChapterEvent
  { _achUuid = fromJust $ U.fromString "6585a64d-c75b-47fc-a86e-e0c8e773528f"
  , _achKmUuid = km1 ^. uuid
  , _achChapterUuid = chapter3WithoutQuestions ^. uuid
  , _achTitle = chapter3WithoutQuestions ^. title
  , _achText = chapter3WithoutQuestions ^. text
  }

e_km1_ch1 :: EditChapterEvent
e_km1_ch1 =
  EditChapterEvent
  { _echUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _echKmUuid = km1 ^. uuid
  , _echChapterUuid = chapter1 ^. uuid
  , _echTitle = Just $ chapter1WithChangeProperties ^. title
  , _echText = Just $ chapter1WithChangeProperties ^. text
  , _echQuestionIds = Just $ questionIds chapter1WithChangeProperties
  }

e_km1_ch1_2 :: EditChapterEvent
e_km1_ch1_2 =
  EditChapterEvent
  { _echUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _echKmUuid = km1 ^. uuid
  , _echChapterUuid = chapter1 ^. uuid
  , _echTitle = Just $ "TWICE: " ++ chapter1WithChangeProperties ^. title
  , _echText = Just $ chapter1WithChangeProperties ^. text
  , _echQuestionIds = Just $ questionIds chapter1WithChangeProperties
  }

d_km1_ch1 :: DeleteChapterEvent
d_km1_ch1 =
  DeleteChapterEvent
  { _dchUuid = fromJust $ U.fromString "d07cc69b-abd3-43ec-bce1-fe59899dbda3"
  , _dchKmUuid = km1 ^. uuid
  , _dchChapterUuid = chapter1 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q1 :: AddQuestionEvent
a_km1_ch1_q1 =
  AddQuestionEvent
  { _aqUuid = fromJust $ U.fromString "71ae2ce9-553b-4ca2-a542-1bce04406c51"
  , _aqKmUuid = km1 ^. uuid
  , _aqChapterUuid = chapter1 ^. uuid
  , _aqQuestionUuid = question1 ^. uuid
  , _aqShortQuestionUuid = question1 ^. shortUuid
  , _aqType = question1 ^. qType
  , _aqTitle = question1 ^. title
  , _aqText = question1 ^. text
  }

a_km1_ch1_q2 :: AddQuestionEvent
a_km1_ch1_q2 =
  AddQuestionEvent
  { _aqUuid = fromJust $ U.fromString "ced9be29-24af-4443-8f5f-e709791a8fe3"
  , _aqKmUuid = km1 ^. uuid
  , _aqChapterUuid = chapter1 ^. uuid
  , _aqQuestionUuid = question2 ^. uuid
  , _aqShortQuestionUuid = question2 ^. shortUuid
  , _aqType = question2 ^. qType
  , _aqTitle = question2 ^. title
  , _aqText = question2 ^. text
  }

a_km1_ch1_q3 :: AddQuestionEvent
a_km1_ch1_q3 =
  AddQuestionEvent
  { _aqUuid = fromJust $ U.fromString "d559ac95-cc81-4502-a780-dbaee46f24bc"
  , _aqKmUuid = km1 ^. uuid
  , _aqChapterUuid = chapter1 ^. uuid
  , _aqQuestionUuid = question3 ^. uuid
  , _aqShortQuestionUuid = question3 ^. shortUuid
  , _aqType = question3 ^. qType
  , _aqTitle = question3 ^. title
  , _aqText = question3 ^. text
  }

a_km1_ch2_q3 :: AddQuestionEvent
a_km1_ch2_q3 =
  AddQuestionEvent
  { _aqUuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
  , _aqKmUuid = km1 ^. uuid
  , _aqChapterUuid = chapter2 ^. uuid
  , _aqQuestionUuid = question3 ^. uuid
  , _aqShortQuestionUuid = question3 ^. shortUuid
  , _aqType = question3 ^. qType
  , _aqTitle = question3 ^. title
  , _aqText = question3 ^. text
  }

e_km1_ch1_q1_title :: EditQuestionEvent
e_km1_ch1_q1_title =
  EditQuestionEvent
  { _eqUuid = fromJust $ U.fromString "de86f82b-aaaf-482e-97c7-c7e93d834cd9"
  , _eqKmUuid = km1 ^. uuid
  , _eqChapterUuid = chapter1 ^. uuid
  , _eqQuestionUuid = question1 ^. uuid
  , _eqShortQuestionUuid = Nothing
  , _eqType = Nothing
  , _eqTitle = Just $ "EDITED: " ++ question2WithChangeProperties ^. title
  , _eqText = Nothing
  , _eqAnswerIds = Nothing
  , _eqExpertIds = Nothing
  , _eqReferenceIds = Nothing
  }

e_km1_ch1_q2 :: EditQuestionEvent
e_km1_ch1_q2 =
  EditQuestionEvent
  { _eqUuid = fromJust $ U.fromString "f56b1435-ec9f-4d79-88b3-04c39b73724d"
  , _eqKmUuid = km1 ^. uuid
  , _eqChapterUuid = chapter1 ^. uuid
  , _eqQuestionUuid = question2 ^. uuid
  , _eqShortQuestionUuid = Just $ question2 ^. shortUuid
  , _eqType = Just $ question2WithChangeProperties ^. qType
  , _eqTitle = Just $ question2WithChangeProperties ^. title
  , _eqText = Just $ question2WithChangeProperties ^. text
  , _eqAnswerIds = Just $ anwerIds question2WithChangeProperties
  , _eqExpertIds = Just $ expertIds question2WithChangeProperties
  , _eqReferenceIds = Just $ referenceIds question2WithChangeProperties
  }

e_km1_ch1_q2_second_edit :: EditQuestionEvent
e_km1_ch1_q2_second_edit =
  EditQuestionEvent
  { _eqUuid = fromJust $ U.fromString "bf888b95-921d-4caa-88af-3309393d44c3"
  , _eqKmUuid = km1 ^. uuid
  , _eqChapterUuid = chapter1 ^. uuid
  , _eqQuestionUuid = question2 ^. uuid
  , _eqShortQuestionUuid = Just $ question2 ^. shortUuid
  , _eqType = Just $ question2WithChangeProperties ^. qType
  , _eqTitle = Just "New title"
  , _eqText = Just $ question2WithChangeProperties ^. text
  , _eqAnswerIds = Just $ anwerIds question2WithChangeProperties
  , _eqExpertIds = Just $ expertIds question2WithChangeProperties
  , _eqReferenceIds = Just $ referenceIds question2WithChangeProperties
  }

d_km1_ch1_q1 :: DeleteQuestionEvent
d_km1_ch1_q1 =
  DeleteQuestionEvent
  { _dqUuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
  , _dqKmUuid = km1 ^. uuid
  , _dqChapterUuid = chapter1 ^. uuid
  , _dqQuestionUuid = question1 ^. uuid
  }

d_km1_ch1_q1_2 :: DeleteQuestionEvent
d_km1_ch1_q1_2 =
  DeleteQuestionEvent
  { _dqUuid = fromJust $ U.fromString "aed9cf13-c81a-481f-bd8a-2689c4a74369"
  , _dqKmUuid = km1 ^. uuid
  , _dqChapterUuid = chapter1 ^. uuid
  , _dqQuestionUuid = question1 ^. uuid
  }

d_km1_ch1_q2 :: DeleteQuestionEvent
d_km1_ch1_q2 =
  DeleteQuestionEvent
  { _dqUuid = fromJust $ U.fromString "52a7a6ae-be37-4075-ac5c-a20858707a75"
  , _dqKmUuid = km1 ^. uuid
  , _dqChapterUuid = chapter1 ^. uuid
  , _dqQuestionUuid = question2 ^. uuid
  }

d_km1_ch1_q3 :: DeleteQuestionEvent
d_km1_ch1_q3 =
  DeleteQuestionEvent
  { _dqUuid = fromJust $ U.fromString "e46d208f-eb7d-48bc-8187-13a72b17ddb2"
  , _dqKmUuid = km1 ^. uuid
  , _dqChapterUuid = chapter1 ^. uuid
  , _dqQuestionUuid = question3 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_aNo1 :: AddAnswerEvent
a_km1_ch1_q2_aNo1 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "afb36736-503a-43ca-a56b-8c144f89809e"
  , _aansKmUuid = km1 ^. uuid
  , _aansChapterUuid = chapter1 ^. uuid
  , _aansQuestionUuid = question2 ^. uuid
  , _aansAnswerUuid = answerNo1 ^. uuid
  , _aansLabel = answerNo1 ^. label
  , _aansAdvice = answerNo1 ^. advice
  }

a_km1_ch1_q2_aYes1 :: AddAnswerEvent
a_km1_ch1_q2_aYes1 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "e7ee93e4-18e7-4748-b0a5-781c77b8c937"
  , _aansKmUuid = km1 ^. uuid
  , _aansChapterUuid = chapter1 ^. uuid
  , _aansQuestionUuid = question2 ^. uuid
  , _aansAnswerUuid = answerYes1 ^. uuid
  , _aansLabel = answerYes1 ^. label
  , _aansAdvice = answerYes1 ^. advice
  }

a_km1_ch1_q2_aMaybe :: AddAnswerEvent
a_km1_ch1_q2_aMaybe =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "8ba60993-96ac-496b-9b8c-9580bf992cab"
  , _aansKmUuid = km1 ^. uuid
  , _aansChapterUuid = chapter1 ^. uuid
  , _aansQuestionUuid = question2 ^. uuid
  , _aansAnswerUuid = answerMaybe ^. uuid
  , _aansLabel = answerMaybe ^. label
  , _aansAdvice = answerMaybe ^. advice
  }

a_km1_ch1_q2_aNo3 :: AddAnswerEvent
a_km1_ch1_q2_aNo3 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "e62168e2-afe5-4e58-8ee7-555594aec23e"
  , _aansKmUuid = km1 ^. uuid
  , _aansChapterUuid = chapter1 ^. uuid
  , _aansQuestionUuid = followUpQuestion1 ^. uuid
  , _aansAnswerUuid = answerNo3 ^. uuid
  , _aansLabel = answerNo3 ^. label
  , _aansAdvice = answerNo3 ^. advice
  }

a_km1_ch1_q2_aYes3 :: AddAnswerEvent
a_km1_ch1_q2_aYes3 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "bc530681-b45b-4d36-b179-a9cb62a92838"
  , _aansKmUuid = km1 ^. uuid
  , _aansChapterUuid = chapter1 ^. uuid
  , _aansQuestionUuid = followUpQuestion1 ^. uuid
  , _aansAnswerUuid = answerYes3 ^. uuid
  , _aansLabel = answerYes3 ^. label
  , _aansAdvice = answerYes3 ^. advice
  }

a_km1_ch1_q2_aNo4 :: AddAnswerEvent
a_km1_ch1_q2_aNo4 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "abf67af9-23e0-43fa-a54a-746570882624"
  , _aansKmUuid = km1 ^. uuid
  , _aansChapterUuid = chapter1 ^. uuid
  , _aansQuestionUuid = followUpQuestion2 ^. uuid
  , _aansAnswerUuid = answerNo4 ^. uuid
  , _aansLabel = answerNo4 ^. label
  , _aansAdvice = answerNo4 ^. advice
  }

a_km1_ch1_q2_aYes4 :: AddAnswerEvent
a_km1_ch1_q2_aYes4 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "542c0d28-9ae3-4bbe-8030-92a78b462276"
  , _aansKmUuid = km1 ^. uuid
  , _aansChapterUuid = chapter1 ^. uuid
  , _aansQuestionUuid = followUpQuestion2 ^. uuid
  , _aansAnswerUuid = answerYes4 ^. uuid
  , _aansLabel = answerYes4 ^. label
  , _aansAdvice = answerYes4 ^. advice
  }

a_km1_ch2_q3_aNo2 :: AddAnswerEvent
a_km1_ch2_q3_aNo2 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "1bb10e82-33b5-4c98-b1d1-ab5413b5df66"
  , _aansKmUuid = km1 ^. uuid
  , _aansChapterUuid = chapter2 ^. uuid
  , _aansQuestionUuid = question3 ^. uuid
  , _aansAnswerUuid = answerNo2 ^. uuid
  , _aansLabel = answerNo2 ^. label
  , _aansAdvice = answerNo2 ^. advice
  }

a_km1_ch2_q3_aYes2 :: AddAnswerEvent
a_km1_ch2_q3_aYes2 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "885ea1b9-0041-4240-911c-f35a9a6e4cbd"
  , _aansKmUuid = km1 ^. uuid
  , _aansChapterUuid = chapter2 ^. uuid
  , _aansQuestionUuid = question3 ^. uuid
  , _aansAnswerUuid = answerYes2 ^. uuid
  , _aansLabel = answerYes2 ^. label
  , _aansAdvice = answerYes2 ^. advice
  }

e_km1_ch1_q2_aYes1 :: EditAnswerEvent
e_km1_ch1_q2_aYes1 =
  EditAnswerEvent
  { _eansUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
  , _eansKmUuid = km1 ^. uuid
  , _eansChapterUuid = chapter1 ^. uuid
  , _eansQuestionUuid = question2 ^. uuid
  , _eansAnswerUuid = answerYes1 ^. uuid
  , _eansLabel = Just $ answerYes1Changed ^. label
  , _eansAdvice = Just $ answerYes1Changed ^. advice
  , _eansFollowUpIds = Just $ followUpIds answerYes1Changed
  }

e_km1_ch1_q2_aYes1_2 :: EditAnswerEvent
e_km1_ch1_q2_aYes1_2 =
  EditAnswerEvent
  { _eansUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
  , _eansKmUuid = km1 ^. uuid
  , _eansChapterUuid = chapter1 ^. uuid
  , _eansQuestionUuid = question2 ^. uuid
  , _eansAnswerUuid = answerYes1 ^. uuid
  , _eansLabel = Just $ answerYes1Changed ^. label
  , _eansAdvice = Just $ answerYes1Changed ^. advice
  , _eansFollowUpIds = Just $ followUpIds answerYes1
  }

d_km1_ch1_q2_aYes1 :: DeleteAnswerEvent
d_km1_ch1_q2_aYes1 =
  DeleteAnswerEvent
  { _dansUuid = fromJust $ U.fromString "1968692f-959a-4d47-b85f-d684eedb3e7f"
  , _dansKmUuid = km1 ^. uuid
  , _dansChapterUuid = chapter1 ^. uuid
  , _dansQuestionUuid = question2 ^. uuid
  , _dansAnswerUuid = answerYes1 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_ansYes1_fuq1 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1 =
  AddFollowUpQuestionEvent
  { _afuqUuid = fromJust $ U.fromString "3588358c-159e-41a9-9847-262611007b61"
  , _afuqKmUuid = km1 ^. uuid
  , _afuqChapterUuid = chapter1 ^. uuid
  , _afuqAnswerUuid = answerYes1 ^. uuid
  , _afuqQuestionUuid = followUpQuestion1 ^. uuid
  , _afuqShortQuestionUuid = followUpQuestion1 ^. shortUuid
  , _afuqType = followUpQuestion1 ^. qType
  , _afuqTitle = followUpQuestion1 ^. title
  , _afuqText = followUpQuestion1 ^. text
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  AddFollowUpQuestionEvent
  { _afuqUuid = fromJust $ U.fromString "8ced5634-a879-4da2-b7c9-158ca6a4e0e3"
  , _afuqKmUuid = km1 ^. uuid
  , _afuqChapterUuid = chapter1 ^. uuid
  , _afuqAnswerUuid = answerYes3 ^. uuid
  , _afuqQuestionUuid = followUpQuestion2 ^. uuid
  , _afuqShortQuestionUuid = followUpQuestion2 ^. shortUuid
  , _afuqType = followUpQuestion2 ^. qType
  , _afuqTitle = followUpQuestion2 ^. title
  , _afuqText = followUpQuestion2 ^. text
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3 =
  AddFollowUpQuestionEvent
  { _afuqUuid = fromJust $ U.fromString "6e9b591f-e6f9-46dd-85e8-a90fe4acc51c"
  , _afuqKmUuid = km1 ^. uuid
  , _afuqChapterUuid = chapter1 ^. uuid
  , _afuqAnswerUuid = answerYes4 ^. uuid
  , _afuqQuestionUuid = followUpQuestion3 ^. uuid
  , _afuqShortQuestionUuid = followUpQuestion3 ^. shortUuid
  , _afuqType = followUpQuestion3 ^. qType
  , _afuqTitle = followUpQuestion3 ^. title
  , _afuqText = followUpQuestion3 ^. text
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: EditFollowUpQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  EditFollowUpQuestionEvent
  { _efuqUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _efuqKmUuid = km1 ^. uuid
  , _efuqChapterUuid = chapter1 ^. uuid
  , _efuqAnswerUuid = answerYes3 ^. uuid
  , _efuqQuestionUuid = followUpQuestion2 ^. uuid
  , _efuqShortQuestionUuid = Just $ followUpQuestion2 ^. shortUuid
  , _efuqType = Just $ followUpQuestion2Changed ^. qType
  , _efuqTitle = Just $ followUpQuestion2Changed ^. title
  , _efuqText = Just $ followUpQuestion2Changed ^. text
  , _efuqAnswerIds = Just $ anwerIds followUpQuestion2Changed
  , _efuqExpertIds = Just $ expertIds followUpQuestion2Changed
  , _efuqReferenceIds = Just $ referenceIds followUpQuestion2Changed
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 :: EditFollowUpQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 =
  EditFollowUpQuestionEvent
  { _efuqUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _efuqKmUuid = km1 ^. uuid
  , _efuqChapterUuid = chapter1 ^. uuid
  , _efuqAnswerUuid = answerYes3 ^. uuid
  , _efuqQuestionUuid = followUpQuestion2 ^. uuid
  , _efuqShortQuestionUuid = Just $ followUpQuestion2 ^. shortUuid
  , _efuqType = Just $ followUpQuestion2Changed ^. qType
  , _efuqTitle = Just $ followUpQuestion2Changed ^. title
  , _efuqText = Just $ followUpQuestion2Changed ^. text
  , _efuqAnswerIds = Just $ [answerYes4 ^. uuid, answerNo4 ^. uuid]
  , _efuqExpertIds = Just $ expertIds followUpQuestion2
  , _efuqReferenceIds = Just $ referenceIds followUpQuestion2
  }

d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: DeleteFollowUpQuestionEvent
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  DeleteFollowUpQuestionEvent
  { _dfuqUuid = fromJust $ U.fromString "db69d694-cfb6-4461-8a13-81c01638f348"
  , _dfuqKmUuid = km1 ^. uuid
  , _dfuqChapterUuid = chapter1 ^. uuid
  , _dfuqAnswerUuid = answerYes3 ^. uuid
  , _dfuqQuestionUuid = followUpQuestion2 ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_eDarth :: AddExpertEvent
a_km1_ch1_q2_eDarth =
  AddExpertEvent
  { _aexpUuid = fromJust $ U.fromString "ec76054f-d059-4a5f-81c9-1817004a913c"
  , _aexpKmUuid = km1 ^. uuid
  , _aexpChapterUuid = chapter1 ^. uuid
  , _aexpQuestionUuid = question2 ^. uuid
  , _aexpExpertUuid = expertDarth ^. uuid
  , _aexpName = expertDarth ^. name
  , _aexpEmail = expertDarth ^. email
  }

a_km1_ch1_q2_eLuke :: AddExpertEvent
a_km1_ch1_q2_eLuke =
  AddExpertEvent
  { _aexpUuid = fromJust $ U.fromString "40bb45bd-4195-4430-ac8f-16ac5a61ece0"
  , _aexpKmUuid = km1 ^. uuid
  , _aexpChapterUuid = chapter1 ^. uuid
  , _aexpQuestionUuid = question2 ^. uuid
  , _aexpExpertUuid = expertLuke ^. uuid
  , _aexpName = expertLuke ^. name
  , _aexpEmail = expertLuke ^. email
  }

a_km1_ch1_q2_eJohn :: AddExpertEvent
a_km1_ch1_q2_eJohn =
  AddExpertEvent
  { _aexpUuid = fromJust $ U.fromString "2d5eedae-1782-44ac-9d4e-3db769161448"
  , _aexpKmUuid = km1 ^. uuid
  , _aexpChapterUuid = chapter1 ^. uuid
  , _aexpQuestionUuid = question2 ^. uuid
  , _aexpExpertUuid = expertJohn ^. uuid
  , _aexpName = expertJohn ^. name
  , _aexpEmail = expertJohn ^. email
  }

e_km1_ch1_q2_eDarth :: EditExpertEvent
e_km1_ch1_q2_eDarth =
  EditExpertEvent
  { _eexpUuid = fromJust $ U.fromString "01686131-2423-4d97-a949-4fea2c9ce3b7"
  , _eexpKmUuid = km1 ^. uuid
  , _eexpChapterUuid = chapter1 ^. uuid
  , _eexpQuestionUuid = question2 ^. uuid
  , _eexpExpertUuid = expertDarth ^. uuid
  , _eexpName = Just $ expertDarthChanged ^. name
  , _eexpEmail = Just $ expertDarthChanged ^. email
  }

d_km1_ch1_q2_eLuke :: DeleteExpertEvent
d_km1_ch1_q2_eLuke =
  DeleteExpertEvent
  { _dexpUuid = fromJust $ U.fromString "f20bc988-6d44-4051-990d-d16b24f369ac"
  , _dexpKmUuid = km1 ^. uuid
  , _dexpChapterUuid = chapter1 ^. uuid
  , _dexpQuestionUuid = question2 ^. uuid
  , _dexpExpertUuid = expertLuke ^. uuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_rCh1 :: AddReferenceEvent
a_km1_ch1_q2_rCh1 =
  AddReferenceEvent
  { _arefUuid = fromJust $ U.fromString "1177d72f-b7d8-466d-ad33-d5f82d0f192a"
  , _arefKmUuid = km1 ^. uuid
  , _arefChapterUuid = chapter1 ^. uuid
  , _arefQuestionUuid = question2 ^. uuid
  , _arefReferenceUuid = referenceCh1 ^. uuid
  , _arefChapter = referenceCh1 ^. chapter
  }

a_km1_ch1_q2_rCh2 :: AddReferenceEvent
a_km1_ch1_q2_rCh2 =
  AddReferenceEvent
  { _arefUuid = fromJust $ U.fromString "4814f50f-8838-4b53-8b18-c0f8c568220e"
  , _arefKmUuid = km1 ^. uuid
  , _arefChapterUuid = chapter1 ^. uuid
  , _arefQuestionUuid = question2 ^. uuid
  , _arefReferenceUuid = referenceCh2 ^. uuid
  , _arefChapter = referenceCh2 ^. chapter
  }

a_km1_ch1_q2_rCh3 :: AddReferenceEvent
a_km1_ch1_q2_rCh3 =
  AddReferenceEvent
  { _arefUuid = fromJust $ U.fromString "45d8ec86-34bc-4e8f-b42a-48a567a77d8b"
  , _arefKmUuid = km1 ^. uuid
  , _arefChapterUuid = chapter1 ^. uuid
  , _arefQuestionUuid = question2 ^. uuid
  , _arefReferenceUuid = referenceCh3 ^. uuid
  , _arefChapter = referenceCh3 ^. chapter
  }

e_km1_ch1_q2_rCh1 :: EditReferenceEvent
e_km1_ch1_q2_rCh1 =
  EditReferenceEvent
  { _erefUuid = fromJust $ U.fromString "08cd9afc-d416-48ab-8669-17e87ceb15dc"
  , _erefKmUuid = km1 ^. uuid
  , _erefChapterUuid = chapter1 ^. uuid
  , _erefQuestionUuid = question2 ^. uuid
  , _erefReferenceUuid = referenceCh1 ^. uuid
  , _erefChapter = Just $ referenceCh1Changed ^. chapter
  }

d_km1_ch1_q2_rCh2 :: DeleteReferenceEvent
d_km1_ch1_q2_rCh2 =
  DeleteReferenceEvent
  { _drefUuid = fromJust $ U.fromString "3cc15f31-4801-404f-ba48-6b91f77d1abe"
  , _drefKmUuid = km1 ^. uuid
  , _drefChapterUuid = chapter1 ^. uuid
  , _drefQuestionUuid = question2 ^. uuid
  , _drefReferenceUuid = referenceCh2 ^. uuid
  }
