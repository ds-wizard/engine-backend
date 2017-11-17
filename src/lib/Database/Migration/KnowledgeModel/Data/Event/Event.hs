module Database.Migration.KnowledgeModel.Data.Event.Event where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.KnowledgeModel.Data.KnowledgeModel.AnswersAndFollowUpQuestions
import Database.Migration.KnowledgeModel.Data.KnowledgeModel.Chapters
import Database.Migration.KnowledgeModel.Data.KnowledgeModel.Experts
import Database.Migration.KnowledgeModel.Data.KnowledgeModel.KnowledgeModels
import Database.Migration.KnowledgeModel.Data.KnowledgeModel.Questions
import Database.Migration.KnowledgeModel.Data.KnowledgeModel.References
import Model.Event.Answer.AddAnswerEvent
import Model.Event.Answer.DeleteAnswerEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Chapter.DeleteChapterEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Common
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

a_km1 :: AddKnowledgeModelEvent
a_km1 =
  AddKnowledgeModelEvent
  { _akmUuid = fromJust $ U.fromString "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f"
  , _akmKmUuid = km1WithoutChapters ^. kmUuid
  , _akmName = km1WithoutChapters ^. kmName
  }

e_km1 :: EditKnowledgeModelEvent
e_km1 =
  EditKnowledgeModelEvent
  { _ekmUuid = fromJust $ U.fromString "8294a55d-642d-416c-879b-5a42a4430c24"
  , _ekmKmUuid = km1 ^. kmUuid
  , _ekmName = Just $ km1WithChangeProperties ^. kmName
  , _ekmChapterIds = Just $ kmChapterIds km1WithChangeProperties
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1 :: AddChapterEvent
a_km1_ch1 =
  AddChapterEvent
  { _achUuid = fromJust $ U.fromString "dedc4a9d-00d9-41b6-8494-a10a238be03b"
  , _achKmUuid = km1 ^. kmUuid
  , _achChapterUuid = chapter1WithoutQuestions ^. chUuid
  , _achTitle = chapter1WithoutQuestions ^. chTitle
  , _achText = chapter1WithoutQuestions ^. chText
  }

a_km1_ch2 :: AddChapterEvent
a_km1_ch2 =
  AddChapterEvent
  { _achUuid = fromJust $ U.fromString "6c4bba6e-864b-4871-98ca-49ac7a3e5eb5"
  , _achKmUuid = km1 ^. kmUuid
  , _achChapterUuid = chapter2WithoutQuestions ^. chUuid
  , _achTitle = chapter2WithoutQuestions ^. chTitle
  , _achText = chapter2WithoutQuestions ^. chText
  }

a_km1_ch3 :: AddChapterEvent
a_km1_ch3 =
  AddChapterEvent
  { _achUuid = fromJust $ U.fromString "6585a64d-c75b-47fc-a86e-e0c8e773528f"
  , _achKmUuid = km1 ^. kmUuid
  , _achChapterUuid = chapter3WithoutQuestions ^. chUuid
  , _achTitle = chapter3WithoutQuestions ^. chTitle
  , _achText = chapter3WithoutQuestions ^. chText
  }

e_km1_ch1 :: EditChapterEvent
e_km1_ch1 =
  EditChapterEvent
  { _echUuid = fromJust $ U.fromString "d4adc3e6-c70e-4277-9d1d-0941db0f0141"
  , _echKmUuid = km1 ^. kmUuid
  , _echChapterUuid = chapter1 ^. chUuid
  , _echTitle = Just $ chapter1WithChangeProperties ^. chTitle
  , _echText = Just $ chapter1WithChangeProperties ^. chText
  , _echQuestionIds = Just $ chQuestionIds chapter1WithChangeProperties
  }

d_km1_ch1 :: DeleteChapterEvent
d_km1_ch1 =
  DeleteChapterEvent
  { _dchUuid = fromJust $ U.fromString "d07cc69b-abd3-43ec-bce1-fe59899dbda3"
  , _dchKmUuid = km1 ^. kmUuid
  , _dchChapterUuid = chapter1 ^. chUuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q1 :: AddQuestionEvent
a_km1_ch1_q1 =
  AddQuestionEvent
  { _aqUuid = fromJust $ U.fromString "71ae2ce9-553b-4ca2-a542-1bce04406c51"
  , _aqKmUuid = km1 ^. kmUuid
  , _aqChapterUuid = chapter1 ^. chUuid
  , _aqQuestionUuid = question1 ^. qUuid
  , _aqShortQuestionUuid = question1 ^. qShortUuid
  , _aqType = question1 ^. qType
  , _aqTitle = question1 ^. qTitle
  , _aqText = question1 ^. qText
  }

a_km1_ch1_q2 :: AddQuestionEvent
a_km1_ch1_q2 =
  AddQuestionEvent
  { _aqUuid = fromJust $ U.fromString "ced9be29-24af-4443-8f5f-e709791a8fe3"
  , _aqKmUuid = km1 ^. kmUuid
  , _aqChapterUuid = chapter1 ^. chUuid
  , _aqQuestionUuid = question2 ^. qUuid
  , _aqShortQuestionUuid = question2 ^. qShortUuid
  , _aqType = question2 ^. qType
  , _aqTitle = question2 ^. qTitle
  , _aqText = question2 ^. qText
  }

a_km1_ch1_q3 :: AddQuestionEvent
a_km1_ch1_q3 =
  AddQuestionEvent
  { _aqUuid = fromJust $ U.fromString "d559ac95-cc81-4502-a780-dbaee46f24bc"
  , _aqKmUuid = km1 ^. kmUuid
  , _aqChapterUuid = chapter1 ^. chUuid
  , _aqQuestionUuid = question3 ^. qUuid
  , _aqShortQuestionUuid = question3 ^. qShortUuid
  , _aqType = question3 ^. qType
  , _aqTitle = question3 ^. qTitle
  , _aqText = question3 ^. qText
  }

a_km1_ch2_q3 :: AddQuestionEvent
a_km1_ch2_q3 =
  AddQuestionEvent
  { _aqUuid = fromJust $ U.fromString "bc994b0f-bee1-4f28-9945-9714b0e559e9"
  , _aqKmUuid = km1 ^. kmUuid
  , _aqChapterUuid = chapter2 ^. chUuid
  , _aqQuestionUuid = question3 ^. qUuid
  , _aqShortQuestionUuid = question3 ^. qShortUuid
  , _aqType = question3 ^. qType
  , _aqTitle = question3 ^. qTitle
  , _aqText = question3 ^. qText
  }

e_km1_ch1_q2 :: EditQuestionEvent
e_km1_ch1_q2 =
  EditQuestionEvent
  { _eqUuid = fromJust $ U.fromString "f56b1435-ec9f-4d79-88b3-04c39b73724d"
  , _eqKmUuid = km1 ^. kmUuid
  , _eqChapterUuid = chapter1 ^. chUuid
  , _eqQuestionUuid = question2 ^. qUuid
  , _eqShortQuestionUuid = Just $ question2 ^. qShortUuid
  , _eqType = Just $ question2WithChangeProperties ^. qType
  , _eqTitle = Just $ question2WithChangeProperties ^. qTitle
  , _eqText = Just $ question2WithChangeProperties ^. qText
  , _eqAnswerIds = Just $ qAnwerIds question2WithChangeProperties
  , _eqExpertIds = Just $ qExpertIds question2WithChangeProperties
  , _eqReferenceIds = Just $ qReferenceIds question2WithChangeProperties
  }

d_km1_ch1_q3 :: DeleteQuestionEvent
d_km1_ch1_q3 =
  DeleteQuestionEvent
  { _dqUuid = fromJust $ U.fromString "e46d208f-eb7d-48bc-8187-13a72b17ddb2"
  , _dqKmUuid = km1 ^. kmUuid
  , _dqChapterUuid = chapter1 ^. chUuid
  , _dqQuestionUuid = question3 ^. qUuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_aNo1 :: AddAnswerEvent
a_km1_ch1_q2_aNo1 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "afb36736-503a-43ca-a56b-8c144f89809e"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = question2 ^. qUuid
  , _aansAnswerUuid = answerNo1 ^. ansUuid
  , _aansLabel = answerNo1 ^. ansLabel
  , _aansAdvice = answerNo1 ^. ansAdvice
  }

a_km1_ch1_q2_aYes1 :: AddAnswerEvent
a_km1_ch1_q2_aYes1 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "e7ee93e4-18e7-4748-b0a5-781c77b8c937"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = question2 ^. qUuid
  , _aansAnswerUuid = answerYes1 ^. ansUuid
  , _aansLabel = answerYes1 ^. ansLabel
  , _aansAdvice = answerYes1 ^. ansAdvice
  }

a_km1_ch1_q2_aMaybe :: AddAnswerEvent
a_km1_ch1_q2_aMaybe =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "8ba60993-96ac-496b-9b8c-9580bf992cab"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = question2 ^. qUuid
  , _aansAnswerUuid = answerMaybe ^. ansUuid
  , _aansLabel = answerMaybe ^. ansLabel
  , _aansAdvice = answerMaybe ^. ansAdvice
  }

a_km1_ch1_q2_aNo3 :: AddAnswerEvent
a_km1_ch1_q2_aNo3 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "e62168e2-afe5-4e58-8ee7-555594aec23e"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = followUpQuestion1 ^. qUuid
  , _aansAnswerUuid = answerNo3 ^. ansUuid
  , _aansLabel = answerNo3 ^. ansLabel
  , _aansAdvice = answerNo3 ^. ansAdvice
  }

a_km1_ch1_q2_aYes3 :: AddAnswerEvent
a_km1_ch1_q2_aYes3 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "bc530681-b45b-4d36-b179-a9cb62a92838"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = followUpQuestion1 ^. qUuid
  , _aansAnswerUuid = answerYes3 ^. ansUuid
  , _aansLabel = answerYes3 ^. ansLabel
  , _aansAdvice = answerYes3 ^. ansAdvice
  }

a_km1_ch1_q2_aNo4 :: AddAnswerEvent
a_km1_ch1_q2_aNo4 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "abf67af9-23e0-43fa-a54a-746570882624"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = followUpQuestion2 ^. qUuid
  , _aansAnswerUuid = answerNo4 ^. ansUuid
  , _aansLabel = answerNo4 ^. ansLabel
  , _aansAdvice = answerNo4 ^. ansAdvice
  }

a_km1_ch1_q2_aYes4 :: AddAnswerEvent
a_km1_ch1_q2_aYes4 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "542c0d28-9ae3-4bbe-8030-92a78b462276"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = followUpQuestion2 ^. qUuid
  , _aansAnswerUuid = answerYes4 ^. ansUuid
  , _aansLabel = answerYes4 ^. ansLabel
  , _aansAdvice = answerYes4 ^. ansAdvice
  }

a_km1_ch2_q3_aNo2 :: AddAnswerEvent
a_km1_ch2_q3_aNo2 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "1bb10e82-33b5-4c98-b1d1-ab5413b5df66"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter2 ^. chUuid
  , _aansQuestionUuid = question3 ^. qUuid
  , _aansAnswerUuid = answerNo2 ^. ansUuid
  , _aansLabel = answerNo2 ^. ansLabel
  , _aansAdvice = answerNo2 ^. ansAdvice
  }

a_km1_ch2_q3_aYes2 :: AddAnswerEvent
a_km1_ch2_q3_aYes2 =
  AddAnswerEvent
  { _aansUuid = fromJust $ U.fromString "885ea1b9-0041-4240-911c-f35a9a6e4cbd"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter2 ^. chUuid
  , _aansQuestionUuid = question3 ^. qUuid
  , _aansAnswerUuid = answerYes2 ^. ansUuid
  , _aansLabel = answerYes2 ^. ansLabel
  , _aansAdvice = answerYes2 ^. ansAdvice
  }

e_km1_ch1_q2_aYes1 :: EditAnswerEvent
e_km1_ch1_q2_aYes1 =
  EditAnswerEvent
  { _eansUuid = fromJust $ U.fromString "8c6632f6-0335-4912-924a-693a87cbe270"
  , _eansKmUuid = km1 ^. kmUuid
  , _eansChapterUuid = chapter1 ^. chUuid
  , _eansQuestionUuid = question2 ^. qUuid
  , _eansAnswerUuid = answerYes1 ^. ansUuid
  , _eansLabel = Just $ answerYes1Changed ^. ansLabel
  , _eansAdvice = Just $ answerYes1Changed ^. ansAdvice
  , _eansFollowingIds = Just $ ansFollowingIds answerYes1Changed
  }

d_km1_ch1_q2_aYes1 :: DeleteAnswerEvent
d_km1_ch1_q2_aYes1 =
  DeleteAnswerEvent
  { _dansUuid = fromJust $ U.fromString "1968692f-959a-4d47-b85f-d684eedb3e7f"
  , _dansKmUuid = km1 ^. kmUuid
  , _dansChapterUuid = chapter1 ^. chUuid
  , _dansQuestionUuid = question2 ^. qUuid
  , _dansAnswerUuid = answerYes1 ^. ansUuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_ansYes1_fuq1 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1 =
  AddFollowUpQuestionEvent
  { _afuqUuid = fromJust $ U.fromString "3588358c-159e-41a9-9847-262611007b61"
  , _afuqKmUuid = km1 ^. kmUuid
  , _afuqChapterUuid = chapter1 ^. chUuid
  , _afuqAnswerUuid = answerYes1 ^. ansUuid
  , _afuqQuestionUuid = followUpQuestion1 ^. qUuid
  , _afuqShortQuestionUuid = followUpQuestion1 ^. qShortUuid
  , _afuqType = followUpQuestion1 ^. qType
  , _afuqTitle = followUpQuestion1 ^. qTitle
  , _afuqText = followUpQuestion1 ^. qText
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  AddFollowUpQuestionEvent
  { _afuqUuid = fromJust $ U.fromString "8ced5634-a879-4da2-b7c9-158ca6a4e0e3"
  , _afuqKmUuid = km1 ^. kmUuid
  , _afuqChapterUuid = chapter1 ^. chUuid
  , _afuqAnswerUuid = answerYes3 ^. ansUuid
  , _afuqQuestionUuid = followUpQuestion2 ^. qUuid
  , _afuqShortQuestionUuid = followUpQuestion2 ^. qShortUuid
  , _afuqType = followUpQuestion2 ^. qType
  , _afuqTitle = followUpQuestion2 ^. qTitle
  , _afuqText = followUpQuestion2 ^. qText
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3 :: AddFollowUpQuestionEvent
a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3 =
  AddFollowUpQuestionEvent
  { _afuqUuid = fromJust $ U.fromString "6e9b591f-e6f9-46dd-85e8-a90fe4acc51c"
  , _afuqKmUuid = km1 ^. kmUuid
  , _afuqChapterUuid = chapter1 ^. chUuid
  , _afuqAnswerUuid = answerYes4 ^. ansUuid
  , _afuqQuestionUuid = followUpQuestion3 ^. qUuid
  , _afuqShortQuestionUuid = followUpQuestion3 ^. qShortUuid
  , _afuqType = followUpQuestion3 ^. qType
  , _afuqTitle = followUpQuestion3 ^. qTitle
  , _afuqText = followUpQuestion3 ^. qText
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: EditFollowUpQuestionEvent
e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  EditFollowUpQuestionEvent
  { _efuqUuid = fromJust $ U.fromString "378f1fb0-e714-400b-a23d-fa939acd3f45"
  , _efuqKmUuid = km1 ^. kmUuid
  , _efuqChapterUuid = chapter1 ^. chUuid
  , _efuqAnswerUuid = answerYes3 ^. ansUuid
  , _efuqQuestionUuid = followUpQuestion2 ^. qUuid
  , _efuqShortQuestionUuid = Just $ followUpQuestion2 ^. qShortUuid
  , _efuqType = Just $ followUpQuestion2Changed ^. qType
  , _efuqTitle = Just $ followUpQuestion2Changed ^. qTitle
  , _efuqText = Just $ followUpQuestion2Changed ^. qText
  , _efuqAnswerIds = Just $ qAnwerIds followUpQuestion2Changed
  , _efuqExpertIds = Just $ qExpertIds followUpQuestion2Changed
  , _efuqReferenceIds = Just $ qReferenceIds followUpQuestion2Changed
  }

d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 :: DeleteFollowUpQuestionEvent
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  DeleteFollowUpQuestionEvent
  { _dfuqUuid = fromJust $ U.fromString "db69d694-cfb6-4461-8a13-81c01638f348"
  , _dfuqKmUuid = km1 ^. kmUuid
  , _dfuqChapterUuid = chapter1 ^. chUuid
  , _dfuqAnswerUuid = answerYes3 ^. ansUuid
  , _dfuqQuestionUuid = followUpQuestion2 ^. qUuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_eDarth :: AddExpertEvent
a_km1_ch1_q2_eDarth =
  AddExpertEvent
  { _aexpUuid = fromJust $ U.fromString "ec76054f-d059-4a5f-81c9-1817004a913c"
  , _aexpKmUuid = km1 ^. kmUuid
  , _aexpChapterUuid = chapter1 ^. chUuid
  , _aexpQuestionUuid = question2 ^. qUuid
  , _aexpExpertUuid = expertDarth ^. expUuid
  , _aexpName = expertDarth ^. expName
  , _aexpEmail = expertDarth ^. expEmail
  }

a_km1_ch1_q2_eLuke :: AddExpertEvent
a_km1_ch1_q2_eLuke =
  AddExpertEvent
  { _aexpUuid = fromJust $ U.fromString "40bb45bd-4195-4430-ac8f-16ac5a61ece0"
  , _aexpKmUuid = km1 ^. kmUuid
  , _aexpChapterUuid = chapter1 ^. chUuid
  , _aexpQuestionUuid = question2 ^. qUuid
  , _aexpExpertUuid = expertLuke ^. expUuid
  , _aexpName = expertLuke ^. expName
  , _aexpEmail = expertLuke ^. expEmail
  }

a_km1_ch1_q2_eJohn :: AddExpertEvent
a_km1_ch1_q2_eJohn =
  AddExpertEvent
  { _aexpUuid = fromJust $ U.fromString "2d5eedae-1782-44ac-9d4e-3db769161448"
  , _aexpKmUuid = km1 ^. kmUuid
  , _aexpChapterUuid = chapter1 ^. chUuid
  , _aexpQuestionUuid = question2 ^. qUuid
  , _aexpExpertUuid = expertJohn ^. expUuid
  , _aexpName = expertJohn ^. expName
  , _aexpEmail = expertJohn ^. expEmail
  }

e_km1_ch1_q2_eDarth :: EditExpertEvent
e_km1_ch1_q2_eDarth =
  EditExpertEvent
  { _eexpUuid = fromJust $ U.fromString "01686131-2423-4d97-a949-4fea2c9ce3b7"
  , _eexpKmUuid = km1 ^. kmUuid
  , _eexpChapterUuid = chapter1 ^. chUuid
  , _eexpQuestionUuid = question2 ^. qUuid
  , _eexpExpertUuid = expertDarth ^. expUuid
  , _eexpName = Just $ expertDarthChanged ^. expName
  , _eexpEmail = Just $ expertDarthChanged ^. expEmail
  }

d_km1_ch1_q2_eLuke :: DeleteExpertEvent
d_km1_ch1_q2_eLuke =
  DeleteExpertEvent
  { _dexpUuid = fromJust $ U.fromString "f20bc988-6d44-4051-990d-d16b24f369ac"
  , _dexpKmUuid = km1 ^. kmUuid
  , _dexpChapterUuid = chapter1 ^. chUuid
  , _dexpQuestionUuid = question2 ^. qUuid
  , _dexpExpertUuid = expertLuke ^. expUuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_rCh1 :: AddReferenceEvent
a_km1_ch1_q2_rCh1 =
  AddReferenceEvent
  { _arefUuid = fromJust $ U.fromString "1177d72f-b7d8-466d-ad33-d5f82d0f192a"
  , _arefKmUuid = km1 ^. kmUuid
  , _arefChapterUuid = chapter1 ^. chUuid
  , _arefQuestionUuid = question2 ^. qUuid
  , _arefReferenceUuid = referenceCh1 ^. refUuid
  , _arefChapter = referenceCh1 ^. refChapter
  }

a_km1_ch1_q2_rCh2 :: AddReferenceEvent
a_km1_ch1_q2_rCh2 =
  AddReferenceEvent
  { _arefUuid = fromJust $ U.fromString "4814f50f-8838-4b53-8b18-c0f8c568220e"
  , _arefKmUuid = km1 ^. kmUuid
  , _arefChapterUuid = chapter1 ^. chUuid
  , _arefQuestionUuid = question2 ^. qUuid
  , _arefReferenceUuid = referenceCh2 ^. refUuid
  , _arefChapter = referenceCh2 ^. refChapter
  }

a_km1_ch1_q2_rCh3 :: AddReferenceEvent
a_km1_ch1_q2_rCh3 =
  AddReferenceEvent
  { _arefUuid = fromJust $ U.fromString "45d8ec86-34bc-4e8f-b42a-48a567a77d8b"
  , _arefKmUuid = km1 ^. kmUuid
  , _arefChapterUuid = chapter1 ^. chUuid
  , _arefQuestionUuid = question2 ^. qUuid
  , _arefReferenceUuid = referenceCh3 ^. refUuid
  , _arefChapter = referenceCh3 ^. refChapter
  }

e_km1_ch1_q2_rCh1 :: EditReferenceEvent
e_km1_ch1_q2_rCh1 =
  EditReferenceEvent
  { _erefUuid = fromJust $ U.fromString "08cd9afc-d416-48ab-8669-17e87ceb15dc"
  , _erefKmUuid = km1 ^. kmUuid
  , _erefChapterUuid = chapter1 ^. chUuid
  , _erefQuestionUuid = question2 ^. qUuid
  , _erefReferenceUuid = referenceCh1 ^. refUuid
  , _erefChapter = Just $ referenceCh1Changed ^. refChapter
  }

d_km1_ch1_q2_rCh2 :: DeleteReferenceEvent
d_km1_ch1_q2_rCh2 =
  DeleteReferenceEvent
  { _drefUuid = fromJust $ U.fromString "3cc15f31-4801-404f-ba48-6b91f77d1abe"
  , _drefKmUuid = km1 ^. kmUuid
  , _drefChapterUuid = chapter1 ^. chUuid
  , _drefQuestionUuid = question2 ^. qUuid
  , _drefReferenceUuid = referenceCh2 ^. refUuid
  }
--import Control.Lens ((^.))
--import Data.Maybe
--import qualified Data.UUID as U
--
--import Model.Event.KnowledgeModel.AddKnowledgeModelEvent
--import Model.Event.Chapter.AddChapterEvent
--import Model.Event.Question.AddQuestionEvent
--import Database.Migration.KnowledgeModel.Data.KnowledgeModel
--addChapter1 = AddChapterEvent
--  { _achUuid = fromJust (U.fromString "73209e91-4b3a-426c-9b92-145015e2346c")
--  , _achKmUuid = akm ^. akmUuid
--  , _achChapterUuid = fromJust (U.fromString "2e4fdc4f-bc4b-4583-a44e-0f94e511873e")
--  , _achTitle = "Design of experiment"
--  , _achText = "Before you decide to embark on any new study, it is nowadays good practice to consider all options to keep the data generation part of your study as limited as possible. It is not because we can generate massive amounts of data that we always need to do so. Creating data with public money is bringing with it the responsibility to treat those data well and (if potentially useful) make them available for re-use by others."
--  }
--
--addQuestion1 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "84a6eaf0-2222-43bc-9b20-c4aea2446a31")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "20a9f96d-5908-4b05-a6cb-96f59dd7853e")
--  , _aqShortQuestionUuid = Just "atq"
--  , _aqType = "option"
--  , _aqTitle = "Is there any pre-existing data?"
--  , _aqText = "Are there any data sets available in the world that are relevant to your planned research?"
--  }
--
--addQuestion2 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "a10f7e43-0f7a-4a62-8806-a0fba3513bb2")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "8f2d0483-b896-4e2e-8d98-fe7d1c845635")
--  , _aqShortQuestionUuid = Just "ezi"
--  , _aqType = "option"
--  , _aqTitle = "Will you be using any pre-existing data (including other people's data)?"
--  , _aqText = "Will you be referring to any earlier measured data, reference data, or data that should be mined from existing literature? Your own data as well as data from others?"
--  }
--
--addQuestion3 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "3e034be1-475b-4564-b032-6aa7c80a8cfc")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "8c30bc1c-bad5-43af-8812-837c21d9bc97")
--  , _aqShortQuestionUuid = Just "quc"
--  , _aqType = "list"
--  , _aqTitle = "What reference data will you use?"
--  , _aqText = "Much of todays data is used in comparison with reference data. A genome for instance is compared with a reference genome to identify genomic variants. If you use reference data, there are several other issues that you should consider. What are the reference data sets that you will use?"
--  }
--
--addQuestion4 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "cdd89683-c494-4fc0-9737-63db91ee317a")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "a22ab5b4-d4f6-4575-a023-286388c3ec4d")
--  , _aqShortQuestionUuid = Just "ckt"
--  , _aqType = "option"
--  , _aqTitle = "Do you know where and how is it available?"
--  , _aqText = "Do you know where the reference data is available, what the conditions for use are, and how to reference it?"
--  }
--
--addQuestion5 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "a6070f1b-36e9-42e9-a1bd-4e84634b6fd1")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "d04fbdc3-dd7c-40ac-903a-82a93296e188")
--  , _aqShortQuestionUuid = Just "jxb"
--  , _aqType = "option"
--  , _aqTitle = "Do you know in what format the reference data is available?"
--  , _aqText = "Do you know the data format of the reference data? Is this suitable for your work? Does it need to be converted?"
--  }
--
--addQuestion6 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "899527d9-de81-4302-9de1-78f8bc61c198")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "f5b6f453-14cd-444b-a735-ea785348301c")
--  , _aqShortQuestionUuid = Just "rgy"
--  , _aqType = "option"
--  , _aqTitle = "Is the reference data resource versioned?"
--  , _aqText = "Many reference data sets evolve over time. If the reference data set changes, this may affect your results. If different versions of a reference data set exist, you need to establish your \"version policy\"."
--  }
--
--addQuestion7 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "6902ba27-d24e-4291-bea2-e38ea709d2f2")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "e27fa6c4-0eda-48bb-bc91-1f5f8b2a0cd8")
--  , _aqShortQuestionUuid = Nothing
--  , _aqType = "text"
--  , _aqTitle = "Which version will you use?"
--  , _aqText = "If there are different versions available, you have to decide with all project partners together which version you will be using. Probably you will go for the latest release as of the date of the start of your research project. However, if you have other data from older projects that need to be merged, you may need to consider using the same release you used for a previous project."
--  }
--
--addQuestion8 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "f6691d5e-6e93-4025-9b13-dc9ea0a78a06")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "e4dc5201-42df-4f3a-b11f-aa05c7b8bcbb")
--  , _aqShortQuestionUuid = Nothing
--  , _aqType = "option"
--  , _aqTitle = "Will you change version if it updates?"
--  , _aqText = "If the reference changes while you are working on your research project, you need to decide whether you will follow these changes. Most likely that will mean that you have to do some analyses again, so you will need to make sure enough resources are available to do so. You can decide to stay with the version that you started with; this can have the disadvantage that you will not benefit from added information or added consistency."
--  }
--
--addQuestion9 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "e8022e65-9041-456e-877c-ca93c8868c83")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "fb1910aa-b693-49f0-8f54-2a38f0a2045f")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion10 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "fa1cef2d-14ad-4ac0-adaa-7d7b6266d392")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "a74d59a8-4a1e-46b9-8fdd-be5d83017ca9")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion12 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion13 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion14 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion15 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion16 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion17 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion18 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion19 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion20 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion21 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion22 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion23 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
--
--addQuestion24 = AddQuestionEvent
--  { _aqUuid = fromJust (U.fromString "")
--  , _aqKmUuid = addChapter1 ^. achKmUuid
--  , _aqChapterUuid = addChapter1 ^. achChapterUuid
--  , _aqQuestionUuid = fromJust (U.fromString "")
--  , _aqShortQuestionUuid = ""
--  , _aqType = ""
--  , _aqTitle = ""
--  , _aqText = ""
--  }
