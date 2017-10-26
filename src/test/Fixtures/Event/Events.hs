module Fixtures.Event.Events where

import Control.Lens

import Fixtures.KnowledgeModel.AnswersAndFollowUpQuestions
import Fixtures.KnowledgeModel.Chapters
import Fixtures.KnowledgeModel.Experts
import Fixtures.KnowledgeModel.KnowledgeModels
import Fixtures.KnowledgeModel.Questions
import Fixtures.KnowledgeModel.References
import KMMigration.Migration.Event.Answer.AddAnswerEvent
import KMMigration.Migration.Event.Answer.DeleteAnswerEvent
import KMMigration.Migration.Event.Answer.EditAnswerEvent
import KMMigration.Migration.Event.Chapter.AddChapterEvent
import KMMigration.Migration.Event.Chapter.DeleteChapterEvent
import KMMigration.Migration.Event.Chapter.EditChapterEvent
import KMMigration.Migration.Event.Common
import KMMigration.Migration.Event.FollowUpQuestion.AddFollowUpQuestionEvent
import KMMigration.Migration.Event.FollowUpQuestion.DeleteFollowUpQuestionEvent
import KMMigration.Migration.Event.FollowUpQuestion.EditFollowUpQuestionEvent
import KMMigration.Migration.Event.Expert.AddExpertEvent
import KMMigration.Migration.Event.Expert.DeleteExpertEvent
import KMMigration.Migration.Event.Expert.EditExpertEvent
import KMMigration.Migration.Event.KnowledgeModel.AddKnowledgeModelEvent
import KMMigration.Migration.Event.KnowledgeModel.EditKnowledgeModelEvent
import KMMigration.Migration.Event.Question.AddQuestionEvent
import KMMigration.Migration.Event.Question.DeleteQuestionEvent
import KMMigration.Migration.Event.Question.EditQuestionEvent
import KMMigration.Migration.Event.Reference.AddReferenceEvent
import KMMigration.Migration.Event.Reference.DeleteReferenceEvent
import KMMigration.Migration.Event.Reference.EditReferenceEvent
import KMMigration.Model.Event
import KMMigration.Model.KnowledgeModel

a_km1 =
  AddKnowledgeModelEvent
  { _akmUuid = "a_km1"
  , _akmKmUuid = km1WithoutChapters ^. kmUuid
  , _akmName = km1WithoutChapters ^. kmName
  }

e_km1 =
  EditKnowledgeModelEvent
  { _ekmUuid = "e_km1"
  , _ekmKmUuid = km1 ^. kmUuid
  , _ekmName = Just $ km1WithChangeProperties ^. kmName
  , _ekmChapterIds = Just $ kmChapterIds km1WithChangeProperties
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1 =
  AddChapterEvent
  { _achUuid = "a_km1_ch1"
  , _achKmUuid = km1 ^. kmUuid
  , _achChapterUuid = chapter1WithoutQuestions ^. chUuid
  , _achTitle = chapter1WithoutQuestions ^. chTitle
  , _achText = chapter1WithoutQuestions ^. chText
  }

a_km1_ch2 =
  AddChapterEvent
  { _achUuid = "a_km1_ch2"
  , _achKmUuid = km1 ^. kmUuid
  , _achChapterUuid = chapter2WithoutQuestions ^. chUuid
  , _achTitle = chapter2WithoutQuestions ^. chTitle
  , _achText = chapter2WithoutQuestions ^. chText
  }

a_km1_ch3 =
  AddChapterEvent
  { _achUuid = "a_km1_ch3"
  , _achKmUuid = km1 ^. kmUuid
  , _achChapterUuid = chapter3WithoutQuestions ^. chUuid
  , _achTitle = chapter3WithoutQuestions ^. chTitle
  , _achText = chapter3WithoutQuestions ^. chText
  }

e_km1_ch1 =
  EditChapterEvent
  { _echUuid = "e_km1_ch1"
  , _echKmUuid = km1 ^. kmUuid
  , _echChapterUuid = chapter1 ^. chUuid
  , _echTitle = Just $ chapter1WithChangeProperties ^. chTitle
  , _echText = Just $ chapter1WithChangeProperties ^. chText
  , _echQuestionIds = Just $ chQuestionIds chapter1WithChangeProperties
  }

d_km1_ch1 =
  DeleteChapterEvent
  { _dchUuid = "d_km1_ch1"
  , _dchKmUuid = km1 ^. kmUuid
  , _dchChapterUuid = chapter1 ^. chUuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q1 =
  AddQuestionEvent
  { _aqUuid = "a_km1_ch1_q1"
  , _aqKmUuid = km1 ^. kmUuid
  , _aqChapterUuid = chapter1 ^. chUuid
  , _aqQuestionUuid = question1 ^. qUuid
  , _aqType = question1 ^. qType
  , _aqTitle = question1 ^. qTitle
  , _aqText = question1 ^. qText
  }

a_km1_ch1_q2 =
  AddQuestionEvent
  { _aqUuid = "a_km1_ch1_q2"
  , _aqKmUuid = km1 ^. kmUuid
  , _aqChapterUuid = chapter1 ^. chUuid
  , _aqQuestionUuid = question2 ^. qUuid
  , _aqType = question2 ^. qType
  , _aqTitle = question2 ^. qTitle
  , _aqText = question2 ^. qText
  }

a_km1_ch1_q3 =
  AddQuestionEvent
  { _aqUuid = "a_km1_ch1_q3"
  , _aqKmUuid = km1 ^. kmUuid
  , _aqChapterUuid = chapter1 ^. chUuid
  , _aqQuestionUuid = question3 ^. qUuid
  , _aqType = question3 ^. qType
  , _aqTitle = question3 ^. qTitle
  , _aqText = question3 ^. qText
  }

a_km1_ch2_q3 =
  AddQuestionEvent
  { _aqUuid = "a_km1_ch2_q3"
  , _aqKmUuid = km1 ^. kmUuid
  , _aqChapterUuid = chapter2 ^. chUuid
  , _aqQuestionUuid = question3 ^. qUuid
  , _aqType = question3 ^. qType
  , _aqTitle = question3 ^. qTitle
  , _aqText = question3 ^. qText
  }

e_km1_ch1_q2 =
  EditQuestionEvent
  { _eqUuid = "e_km1_ch1_q2"
  , _eqKmUuid = km1 ^. kmUuid
  , _eqChapterUuid = chapter1 ^. chUuid
  , _eqQuestionUuid = question2 ^. qUuid
  , _eqType = Just $ question2WithChangeProperties ^. qType
  , _eqTitle = Just $ question2WithChangeProperties ^. qTitle
  , _eqText = Just $ question2WithChangeProperties ^. qText
  , _eqAnswerIds = Just $ qAnwerIds question2WithChangeProperties
  , _eqExpertIds = Just $ qExpertIds question2WithChangeProperties
  , _eqReferenceIds = Just $ qReferenceIds question2WithChangeProperties
  }

d_km1_ch1_q3 =
  DeleteQuestionEvent
  { _dqUuid = "d_km1_ch1_q3"
  , _dqKmUuid = km1 ^. kmUuid
  , _dqChapterUuid = chapter1 ^. chUuid
  , _dqQuestionUuid = question3 ^. qUuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_aNo1 =
  AddAnswerEvent
  { _aansUuid = "a_km1_ch1_q2_aNo1"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = question2 ^. qUuid
  , _aansAnswerUuid = answerNo1 ^. ansUuid
  , _aansLabel = answerNo1 ^. ansLabel
  , _aansAdvice = answerNo1 ^. ansAdvice
  }

a_km1_ch1_q2_aYes1 =
  AddAnswerEvent
  { _aansUuid = "a_km1_ch1_q2_aYes1"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = question2 ^. qUuid
  , _aansAnswerUuid = answerYes1 ^. ansUuid
  , _aansLabel = answerYes1 ^. ansLabel
  , _aansAdvice = answerYes1 ^. ansAdvice
  }

a_km1_ch1_q2_aMaybe =
  AddAnswerEvent
  { _aansUuid = "a_km1_ch1_q2_aMaybe"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = question2 ^. qUuid
  , _aansAnswerUuid = answerMaybe ^. ansUuid
  , _aansLabel = answerMaybe ^. ansLabel
  , _aansAdvice = answerMaybe ^. ansAdvice
  }

a_km1_ch1_q2_aNo3 =
  AddAnswerEvent
  { _aansUuid = "a_km1_ch1_q2_aNo3"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = followUpQuestion1 ^. qUuid
  , _aansAnswerUuid = answerNo3 ^. ansUuid
  , _aansLabel = answerNo3 ^. ansLabel
  , _aansAdvice = answerNo3 ^. ansAdvice
  }

a_km1_ch1_q2_aYes3 =
  AddAnswerEvent
  { _aansUuid = "a_km1_ch1_q2_aYes3"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = followUpQuestion1 ^. qUuid
  , _aansAnswerUuid = answerYes3 ^. ansUuid
  , _aansLabel = answerYes3 ^. ansLabel
  , _aansAdvice = answerYes3 ^. ansAdvice
  }

a_km1_ch1_q2_aNo4 =
  AddAnswerEvent
  { _aansUuid = "a_km1_ch1_q2_aNo4"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = followUpQuestion2 ^. qUuid
  , _aansAnswerUuid = answerNo4 ^. ansUuid
  , _aansLabel = answerNo4 ^. ansLabel
  , _aansAdvice = answerNo4 ^. ansAdvice
  }

a_km1_ch1_q2_aYes4 =
  AddAnswerEvent
  { _aansUuid = "a_km1_ch1_q2_aYes4"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter1 ^. chUuid
  , _aansQuestionUuid = followUpQuestion2 ^. qUuid
  , _aansAnswerUuid = answerYes4 ^. ansUuid
  , _aansLabel = answerYes4 ^. ansLabel
  , _aansAdvice = answerYes4 ^. ansAdvice
  }  

a_km1_ch2_q3_aNo2 =
  AddAnswerEvent
  { _aansUuid = "a_km1_ch2_q3_aNo2"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter2 ^. chUuid
  , _aansQuestionUuid = question3 ^. qUuid
  , _aansAnswerUuid = answerNo2 ^. ansUuid
  , _aansLabel = answerNo2 ^. ansLabel
  , _aansAdvice = answerNo2 ^. ansAdvice
  }

a_km1_ch2_q3_aYes2 =
  AddAnswerEvent
  { _aansUuid = "a_km1_ch2_q3_aYes2"
  , _aansKmUuid = km1 ^. kmUuid
  , _aansChapterUuid = chapter2 ^. chUuid
  , _aansQuestionUuid = question3 ^. qUuid
  , _aansAnswerUuid = answerYes2 ^. ansUuid
  , _aansLabel = answerYes2 ^. ansLabel
  , _aansAdvice = answerYes2 ^. ansAdvice
  }

e_km1_ch1_q2_aYes1 =
  EditAnswerEvent
  { _eansUuid = "e_km1_ch1_q2_aYes1"
  , _eansKmUuid = km1 ^. kmUuid
  , _eansChapterUuid = chapter1 ^. chUuid
  , _eansQuestionUuid = question2 ^. qUuid
  , _eansAnswerUuid = answerYes1 ^. ansUuid
  , _eansLabel = Just $ answerYes1Changed ^. ansLabel
  , _eansAdvice = Just $ answerYes1Changed ^. ansAdvice
  , _eansFollowingIds = Just $ ansFollowingIds answerYes1Changed
  }

d_km1_ch1_q2_aYes1 =
  DeleteAnswerEvent
  { _dansUuid = "d_km1_ch1_q2_aYes1"
  , _dansKmUuid = km1 ^. kmUuid
  , _dansChapterUuid = chapter1 ^. chUuid
  , _dansQuestionUuid = question2 ^. qUuid
  , _dansAnswerUuid = answerYes1 ^. ansUuid
  }

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_ansYes1_fuq1 =
  AddFollowUpQuestionEvent
  { _afuqUuid = "a_km1_ch1_ansYes1_fuq1"
  , _afuqKmUuid = km1 ^. kmUuid
  , _afuqChapterUuid = chapter1 ^. chUuid
  , _afuqAnswerUuid = answerYes1 ^. ansUuid
  , _afuqQuestionUuid = followUpQuestion1 ^. qUuid
  , _afuqType = followUpQuestion1 ^. qType
  , _afuqTitle = followUpQuestion1 ^. qTitle
  , _afuqText = followUpQuestion1 ^. qText
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  AddFollowUpQuestionEvent
  { _afuqUuid = "a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2"
  , _afuqKmUuid = km1 ^. kmUuid
  , _afuqChapterUuid = chapter1 ^. chUuid
  , _afuqAnswerUuid = answerYes3 ^. ansUuid
  , _afuqQuestionUuid = followUpQuestion2 ^. qUuid
  , _afuqType = followUpQuestion2 ^. qType
  , _afuqTitle = followUpQuestion2 ^. qTitle
  , _afuqText = followUpQuestion2 ^. qText
  }

a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3 =
  AddFollowUpQuestionEvent
  { _afuqUuid = "a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3"
  , _afuqKmUuid = km1 ^. kmUuid
  , _afuqChapterUuid = chapter1 ^. chUuid
  , _afuqAnswerUuid = answerYes4 ^. ansUuid
  , _afuqQuestionUuid = followUpQuestion3 ^. qUuid
  , _afuqType = followUpQuestion3 ^. qType
  , _afuqTitle = followUpQuestion3 ^. qTitle
  , _afuqText = followUpQuestion3 ^. qText
  }

e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  EditFollowUpQuestionEvent
  { _efuqUuid = "e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2"
  , _efuqKmUuid = km1 ^. kmUuid
  , _efuqChapterUuid = chapter1 ^. chUuid
  , _efuqAnswerUuid = answerYes3 ^. ansUuid
  , _efuqQuestionUuid = followUpQuestion2 ^. qUuid
  , _efuqType = Just $ followUpQuestion2Changed ^. qType
  , _efuqTitle = Just $ followUpQuestion2Changed ^. qTitle
  , _efuqText = Just $ followUpQuestion2Changed ^. qText
  , _efuqAnswerIds = Just $ qAnwerIds followUpQuestion2Changed
  , _efuqExpertIds = Just $ qExpertIds followUpQuestion2Changed
  , _efuqReferenceIds = Just $ qReferenceIds followUpQuestion2Changed
  }
d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2 =
  DeleteFollowUpQuestionEvent
  { _dfuqUuid = "d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2"
  , _dfuqKmUuid = km1 ^. kmUuid
  , _dfuqChapterUuid = chapter1 ^. chUuid
  , _dfuqAnswerUuid = answerYes3 ^. ansUuid
  , _dfuqQuestionUuid = followUpQuestion2 ^. qUuid
  }
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_eDarth =
  AddExpertEvent
  { _aexpUuid = "a_km1_ch1_q2_eDarth"
  , _aexpKmUuid = km1 ^. kmUuid
  , _aexpChapterUuid = chapter1 ^. chUuid
  , _aexpQuestionUuid = question2 ^. qUuid
  , _aexpExpertUuid = expertDarth ^. expUuid
  , _aexpName = expertDarth ^. expName
  , _aexpEmail = expertDarth ^. expEmail
  }
a_km1_ch1_q2_eLuke =
  AddExpertEvent
  { _aexpUuid = "a_km1_ch1_q2_eLuke"
  , _aexpKmUuid = km1 ^. kmUuid
  , _aexpChapterUuid = chapter1 ^. chUuid
  , _aexpQuestionUuid = question2 ^. qUuid
  , _aexpExpertUuid = expertLuke ^. expUuid
  , _aexpName = expertLuke ^. expName
  , _aexpEmail = expertLuke ^. expEmail
  }
a_km1_ch1_q2_eJohn =
  AddExpertEvent
  { _aexpUuid = "a_km1_ch1_q2_eJohn"
  , _aexpKmUuid = km1 ^. kmUuid
  , _aexpChapterUuid = chapter1 ^. chUuid
  , _aexpQuestionUuid = question2 ^. qUuid
  , _aexpExpertUuid = expertJohn ^. expUuid
  , _aexpName = expertJohn ^. expName
  , _aexpEmail = expertJohn ^. expEmail
  }
e_km1_ch1_q2_eDarth =
  EditExpertEvent
  { _eexpUuid = "e_km1_ch1_q2_eDarth"
  , _eexpKmUuid = km1 ^. kmUuid
  , _eexpChapterUuid = chapter1 ^. chUuid
  , _eexpQuestionUuid = question2 ^. qUuid
  , _eexpExpertUuid = expertDarth ^. expUuid
  , _eexpName = Just $ expertDarthChanged ^. expName
  , _eexpEmail = Just $ expertDarthChanged ^. expEmail
  }
d_km1_ch1_q2_eLuke =
  DeleteExpertEvent
  { _dexpUuid = "d_km1_ch1_q2_eLuke"
  , _dexpKmUuid = km1 ^. kmUuid
  , _dexpChapterUuid = chapter1 ^. chUuid
  , _dexpQuestionUuid = question2 ^. qUuid
  , _dexpExpertUuid = expertLuke ^. expUuid
  }
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
a_km1_ch1_q2_rCh1 =
  AddReferenceEvent
  { _arefUuid = "a_km1_ch1_q2_rCh1"
  , _arefKmUuid = km1 ^. kmUuid
  , _arefChapterUuid = chapter1 ^. chUuid
  , _arefQuestionUuid = question2 ^. qUuid
  , _arefReferenceUuid = referenceCh1 ^. refUuid
  , _arefChapter = referenceCh1 ^. refChapter
  }
a_km1_ch1_q2_rCh2 =
  AddReferenceEvent
  { _arefUuid = "a_km1_ch1_q2_rCh2"
  , _arefKmUuid = km1 ^. kmUuid
  , _arefChapterUuid = chapter1 ^. chUuid
  , _arefQuestionUuid = question2 ^. qUuid
  , _arefReferenceUuid = referenceCh2 ^. refUuid
  , _arefChapter = referenceCh2 ^. refChapter
  }
a_km1_ch1_q2_rCh3 =
  AddReferenceEvent
  { _arefUuid = "a_km1_ch1_q2_rCh3"
  , _arefKmUuid = km1 ^. kmUuid
  , _arefChapterUuid = chapter1 ^. chUuid
  , _arefQuestionUuid = question2 ^. qUuid
  , _arefReferenceUuid = referenceCh3 ^. refUuid
  , _arefChapter = referenceCh3 ^. refChapter
  }
e_km1_ch1_q2_rCh1 =
  EditReferenceEvent
  { _erefUuid = "e_km1_ch1_q2_rCh1"
  , _erefKmUuid = km1 ^. kmUuid
  , _erefChapterUuid = chapter1 ^. chUuid
  , _erefQuestionUuid = question2 ^. qUuid
  , _erefReferenceUuid = referenceCh1 ^. refUuid
  , _erefChapter = Just $ referenceCh1Changed ^. refChapter
  }
d_km1_ch1_q2_rCh2 =
  DeleteReferenceEvent
  { _drefUuid = "d_km1_ch1_q2_rCh2"
  , _drefKmUuid = km1 ^. kmUuid
  , _drefChapterUuid = chapter1 ^. chUuid
  , _drefQuestionUuid = question2 ^. qUuid
  , _drefReferenceUuid = referenceCh2 ^. refUuid
  }
