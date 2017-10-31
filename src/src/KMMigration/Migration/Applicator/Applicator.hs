module KMMigration.Migration.Applicator.Applicator where

import Control.Lens

import Model.Event.Answer.AddAnswerEvent
import Model.Event.Answer.DeleteAnswerEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Chapter.DeleteChapterEvent
import Model.Event.Chapter.EditChapterEvent
import KMMigration.Migration.Event.Common
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
import KMMigration.Model.Common
import Model.KnowledgeModel.KnowledgeModel

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToKM e where
  applyEventToKM :: e -> KnowledgeModel -> KnowledgeModel

passToChapters e km = km & kmChapters .~ modifiedChapters
  where
    modifiedChapters = fmap (applyEventToChapter e) (km ^. kmChapters)

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToKM AddKnowledgeModelEvent where
  applyEventToKM e _ =
    KnowledgeModel
    {_kmUuid = e ^. akmKmUuid, _kmName = e ^. akmName, _kmChapters = []}

instance ApplyEventToKM EditKnowledgeModelEvent where
  applyEventToKM e = applyChapterIds . applyName
    where
      applyName km = applyValue (e ^. ekmName) km kmName
      applyChapterIds km =
        applyValue (e ^. ekmChapterIds) km kmChangeChapterIdsOrder

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToKM AddChapterEvent where
  applyEventToKM e km = km & kmChapters .~ modifiedChapters
    where
      modifiedChapters = km ^. kmChapters ++ [newChapter]
      newChapter =
        Chapter
        { _chUuid = e ^. achChapterUuid
        , _chNamespace = "core"
        , _chFormatVersion = 1
        , _chTitle = e ^. achTitle
        , _chText = e ^. achText
        , _chQuestions = []
        }

instance ApplyEventToKM EditChapterEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM DeleteChapterEvent where
  applyEventToKM e km =
    if equalsUuid e km
      then km & kmChapters .~ modifiedChapters
      else km
    where
      modifiedChapters = filter (not . equalsUuid e) (km ^. kmChapters)

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToKM AddQuestionEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM EditQuestionEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM DeleteQuestionEvent where
  applyEventToKM = passToChapters

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToKM AddAnswerEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM EditAnswerEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM DeleteAnswerEvent where
  applyEventToKM = passToChapters

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToKM AddFollowUpQuestionEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM EditFollowUpQuestionEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM DeleteFollowUpQuestionEvent where
  applyEventToKM = passToChapters

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToKM AddExpertEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM EditExpertEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM DeleteExpertEvent where
  applyEventToKM = passToChapters

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToKM AddReferenceEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM EditReferenceEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM DeleteReferenceEvent where
  applyEventToKM = passToChapters

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToChapter e where
  applyEventToChapter :: e -> Chapter -> Chapter

passToQuestions e ch = ch & chQuestions .~ modifiedQuestions
  where
    modifiedQuestions = fmap (applyEventToQuestion e) (ch ^. chQuestions)

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToChapter AddKnowledgeModelEvent where
  applyEventToChapter _ _ = undefined

instance ApplyEventToChapter EditKnowledgeModelEvent where
  applyEventToChapter _ _ = undefined

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToChapter AddChapterEvent where
  applyEventToChapter _ _ = undefined

instance ApplyEventToChapter EditChapterEvent where
  applyEventToChapter e ch =
    if equalsUuid e ch
      then applyQuestionIds . applyText . applyTitle $ ch
      else ch
    where
      applyTitle ch = applyValue (e ^. echTitle) ch chTitle
      applyText ch = applyValue (e ^. echText) ch chText
      applyQuestionIds ch =
        applyValue (e ^. echQuestionIds) ch chChangeQuestionIdsOrder

instance ApplyEventToChapter DeleteChapterEvent where
  applyEventToChapter _ _ = undefined

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToChapter AddQuestionEvent where
  applyEventToChapter e ch =
    if equalsUuid e ch
      then ch & chQuestions .~ modifiedQuestions
      else ch
    where
      modifiedQuestions = ch ^. chQuestions ++ [newQuestion]
      newQuestion =
        Question
        { _qUuid = e ^. aqQuestionUuid
        , _qType = e ^. aqType
        , _qTitle = e ^. aqTitle
        , _qText = e ^. aqText
        , _qAnswers = []
        , _qReferences = []
        , _qExperts = []
        }

instance ApplyEventToChapter EditQuestionEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter DeleteQuestionEvent where
  applyEventToChapter e ch =
    if equalsUuid e ch
      then ch & chQuestions .~ modifiedQuestions
      else passToQuestions e ch
    where
      modifiedQuestions = filter (not . equalsUuid e) (ch ^. chQuestions)

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToChapter AddAnswerEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter EditAnswerEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter DeleteAnswerEvent where
  applyEventToChapter = passToQuestions

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToChapter AddFollowUpQuestionEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter EditFollowUpQuestionEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter DeleteFollowUpQuestionEvent where
  applyEventToChapter = passToQuestions

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToChapter AddExpertEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter EditExpertEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter DeleteExpertEvent where
  applyEventToChapter = passToQuestions

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToChapter AddReferenceEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter EditReferenceEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter DeleteReferenceEvent where
  applyEventToChapter = passToQuestions

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToQuestion e where
  applyEventToQuestion :: e -> Question -> Question

passToAnswers e q = q & qAnswers .~ modifiedAnswers
  where
    modifiedAnswers = fmap (applyEventToAnswer e) (q ^. qAnswers)

passToExperts e q = q & qExperts .~ modifiedExperts
  where
    modifiedExperts = fmap (applyEventToExpert e) (q ^. qExperts)

passToReferences e q = q & qReferences .~ modifiedReferences
  where
    modifiedReferences = fmap (applyEventToReference e) (q ^. qReferences)
  -- -------------------------

-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToQuestion AddKnowledgeModelEvent where
  applyEventToQuestion _ _ = undefined

instance ApplyEventToQuestion EditKnowledgeModelEvent where
  applyEventToQuestion _ _ = undefined

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToQuestion AddChapterEvent where
  applyEventToQuestion _ _ = undefined

instance ApplyEventToQuestion EditChapterEvent where
  applyEventToQuestion _ _ = undefined

instance ApplyEventToQuestion DeleteChapterEvent where
  applyEventToQuestion _ _ = undefined

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToQuestion AddQuestionEvent where
  applyEventToQuestion = passToAnswers

instance ApplyEventToQuestion EditQuestionEvent where
  applyEventToQuestion e q =
    if equalsUuid e q
      then applyReferenceIds .
           applyExpertIds . applyAnwerIds . applyText . applyTitle . applyType $
           q
      else passToAnswers e q
    where
      applyType q = applyValue (e ^. eqType) q qType
      applyTitle q = applyValue (e ^. eqTitle) q qTitle
      applyText q = applyValue (e ^. eqText) q qText
      applyAnwerIds q = applyValue (e ^. eqAnswerIds) q qChangeAnwerIdsOrder
      applyExpertIds q = applyValue (e ^. eqExpertIds) q qChangeExpertIdsOrder
      applyReferenceIds q =
        applyValue (e ^. eqReferenceIds) q qChangeReferenceIdsOrder

instance ApplyEventToQuestion DeleteQuestionEvent where
  applyEventToQuestion = passToAnswers

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToQuestion AddAnswerEvent where
  applyEventToQuestion e q =
    if equalsUuid e q
      then q & qAnswers .~ modifiedAnswers
      else passToAnswers e q
    where
      modifiedAnswers = q ^. qAnswers ++ [newAnswer]
      newAnswer =
        Answer
        { _ansUuid = e ^. aansAnswerUuid
        , _ansLabel = e ^. aansLabel
        , _ansAdvice = e ^. aansAdvice
        , _ansFollowing = []
        }

instance ApplyEventToQuestion EditAnswerEvent where
  applyEventToQuestion = passToAnswers

instance ApplyEventToQuestion DeleteAnswerEvent where
  applyEventToQuestion e q =
    if equalsUuid e q
      then q & qAnswers .~ modifiedAnswers
      else passToAnswers e q
    where
      modifiedAnswers = filter (not . equalsUuid e) (q ^. qAnswers)

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToQuestion AddFollowUpQuestionEvent where
  applyEventToQuestion = passToAnswers

instance ApplyEventToQuestion EditFollowUpQuestionEvent where
  applyEventToQuestion e q =
    if equalsUuid e q
      then applyReferenceIds .
           applyExpertIds . applyAnwerIds . applyText . applyTitle . applyType $
           q
      else passToAnswers e q
    where
      applyType q = applyValue (e ^. efuqType) q qType
      applyTitle q = applyValue (e ^. efuqTitle) q qTitle
      applyText q = applyValue (e ^. efuqText) q qText
      applyAnwerIds q = applyValue (e ^. efuqAnswerIds) q qChangeAnwerIdsOrder
      applyExpertIds q = applyValue (e ^. efuqExpertIds) q qChangeExpertIdsOrder
      applyReferenceIds q =
        applyValue (e ^. efuqReferenceIds) q qChangeReferenceIdsOrder

instance ApplyEventToQuestion DeleteFollowUpQuestionEvent where
  applyEventToQuestion = passToAnswers

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToQuestion AddExpertEvent where
  applyEventToQuestion e q =
    if equalsUuid e q
      then q & qExperts .~ modifiedExperts
      else passToAnswers e q
    where
      modifiedExperts = q ^. qExperts ++ [newExpert]
      newExpert =
        Expert
        { _expUuid = e ^. aexpExpertUuid
        , _expName = e ^. aexpName
        , _expEmail = e ^. aexpEmail
        }

instance ApplyEventToQuestion EditExpertEvent where
  applyEventToQuestion e =
    passToReferences e . passToExperts e . passToAnswers e

instance ApplyEventToQuestion DeleteExpertEvent where
  applyEventToQuestion e q =
    if equalsUuid e q
      then q & qExperts .~ modifiedExperts
      else passToAnswers e q
    where
      modifiedExperts = filter (not . equalsUuid e) (q ^. qExperts)

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToQuestion AddReferenceEvent where
  applyEventToQuestion e q =
    if equalsUuid e q
      then q & qReferences .~ modifiedReferences
      else passToAnswers e q
    where
      modifiedReferences = q ^. qReferences ++ [newReference]
      newReference =
        Reference
        {_refUuid = e ^. arefReferenceUuid, _refChapter = e ^. arefChapter}

instance ApplyEventToQuestion EditReferenceEvent where
  applyEventToQuestion e =
    passToReferences e . passToExperts e . passToAnswers e

instance ApplyEventToQuestion DeleteReferenceEvent where
  applyEventToQuestion e q =
    if equalsUuid e q
      then q & qReferences .~ modifiedReferences
      else passToAnswers e q
    where
      modifiedReferences = filter (not . equalsUuid e) (q ^. qReferences)

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToAnswer e where
  applyEventToAnswer :: e -> Answer -> Answer

passToFollowing e ans = ans & ansFollowing .~ modifiedFollowing
  where
    modifiedFollowing = fmap (applyEventToQuestion e) (ans ^. ansFollowing)

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToAnswer AddKnowledgeModelEvent where
  applyEventToAnswer _ _ = undefined

instance ApplyEventToAnswer EditKnowledgeModelEvent where
  applyEventToAnswer _ _ = undefined

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToAnswer AddChapterEvent where
  applyEventToAnswer _ _ = undefined

instance ApplyEventToAnswer EditChapterEvent where
  applyEventToAnswer _ _ = undefined

instance ApplyEventToAnswer DeleteChapterEvent where
  applyEventToAnswer _ _ = undefined

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToAnswer AddQuestionEvent where
  applyEventToAnswer = passToFollowing

instance ApplyEventToAnswer EditQuestionEvent where
  applyEventToAnswer = passToFollowing

instance ApplyEventToAnswer DeleteQuestionEvent where
  applyEventToAnswer = passToFollowing

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToAnswer AddAnswerEvent where
  applyEventToAnswer = passToFollowing

instance ApplyEventToAnswer EditAnswerEvent where
  applyEventToAnswer e ans =
    if equalsUuid e ans
      then applyFollowing . applyAdvice . applyLabel $ ans
      else passToFollowing e ans
    where
      applyLabel ans = applyValue (e ^. eansLabel) ans ansLabel
      applyAdvice ans = applyValue (e ^. eansAdvice) ans ansAdvice
      applyFollowing ans =
        applyValue (e ^. eansFollowingIds) ans ansChangeFollowingIdsOrder

instance ApplyEventToAnswer DeleteAnswerEvent where
  applyEventToAnswer = passToFollowing

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToAnswer AddFollowUpQuestionEvent where
  applyEventToAnswer e ans =
    if equalsUuid e ans
      then ans & ansFollowing .~ modifiedFollowing
      else passToFollowing e ans
    where
      modifiedFollowing = ans ^. ansFollowing ++ [newFollowing]
      newFollowing =
        Question
        { _qUuid = e ^. afuqQuestionUuid
        , _qType = e ^. afuqType
        , _qTitle = e ^. afuqTitle
        , _qText = e ^. afuqText
        , _qAnswers = []
        , _qReferences = []
        , _qExperts = []
        }

instance ApplyEventToAnswer EditFollowUpQuestionEvent where
  applyEventToAnswer = passToFollowing

instance ApplyEventToAnswer DeleteFollowUpQuestionEvent where
  applyEventToAnswer e ans =
    if equalsUuid e ans
      then ans & ansFollowing .~ modifiedFollowing
      else passToFollowing e ans
    where
      modifiedFollowing = filter (not . equalsUuid e) (ans ^. ansFollowing)

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToAnswer AddExpertEvent where
  applyEventToAnswer = passToFollowing

instance ApplyEventToAnswer EditExpertEvent where
  applyEventToAnswer = passToFollowing

instance ApplyEventToAnswer DeleteExpertEvent where
  applyEventToAnswer = passToFollowing

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToAnswer AddReferenceEvent where
  applyEventToAnswer = passToFollowing

instance ApplyEventToAnswer EditReferenceEvent where
  applyEventToAnswer = passToFollowing

instance ApplyEventToAnswer DeleteReferenceEvent where
  applyEventToAnswer = passToFollowing

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToExpert e where
  applyEventToExpert :: e -> Expert -> Expert

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToExpert AddKnowledgeModelEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert EditKnowledgeModelEvent where
  applyEventToExpert _ _ = undefined

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToExpert AddChapterEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert EditChapterEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert DeleteChapterEvent where
  applyEventToExpert _ _ = undefined

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToExpert AddQuestionEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert EditQuestionEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert DeleteQuestionEvent where
  applyEventToExpert _ _ = undefined

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToExpert AddAnswerEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert EditAnswerEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert DeleteAnswerEvent where
  applyEventToExpert _ _ = undefined

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToExpert AddFollowUpQuestionEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert EditFollowUpQuestionEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert DeleteFollowUpQuestionEvent where
  applyEventToExpert _ _ = undefined

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToExpert AddExpertEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert EditExpertEvent where
  applyEventToExpert e exp =
    if equalsUuid e exp
      then applyEmail . applyName $ exp
      else exp
    where
      applyName exp = applyValue (e ^. eexpName) exp expName
      applyEmail exp = applyValue (e ^. eexpEmail) exp expEmail

instance ApplyEventToExpert DeleteExpertEvent where
  applyEventToExpert _ _ = undefined

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToExpert AddReferenceEvent where
  applyEventToExpert _ _ = undefined

instance ApplyEventToExpert EditReferenceEvent where
  applyEventToExpert e exp = exp

instance ApplyEventToExpert DeleteReferenceEvent where
  applyEventToExpert _ _ = undefined

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToReference e where
  applyEventToReference :: e -> Reference -> Reference

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToReference AddKnowledgeModelEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference EditKnowledgeModelEvent where
  applyEventToReference _ _ = undefined

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToReference AddChapterEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference EditChapterEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference DeleteChapterEvent where
  applyEventToReference _ _ = undefined

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToReference AddQuestionEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference EditQuestionEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference DeleteQuestionEvent where
  applyEventToReference _ _ = undefined

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToReference AddAnswerEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference EditAnswerEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference DeleteAnswerEvent where
  applyEventToReference _ _ = undefined

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToReference AddFollowUpQuestionEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference EditFollowUpQuestionEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference DeleteFollowUpQuestionEvent where
  applyEventToReference _ _ = undefined

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToReference AddExpertEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference EditExpertEvent where
  applyEventToReference e ref = ref

instance ApplyEventToReference DeleteExpertEvent where
  applyEventToReference _ _ = undefined

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToReference AddReferenceEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference EditReferenceEvent where
  applyEventToReference e ref =
    if equalsUuid e ref
      then applyChapter ref
      else ref
    where
      applyChapter ref = applyValue (e ^. erefChapter) ref refChapter

instance ApplyEventToReference DeleteReferenceEvent where
  applyEventToReference _ _ = undefined
