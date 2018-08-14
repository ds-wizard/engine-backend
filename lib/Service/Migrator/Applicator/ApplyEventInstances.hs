module Service.Migrator.Applicator.ApplyEventInstances where

import Control.Lens

import LensesConfig
import Localization
import Model.Error.Error
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventAccessors
import Model.Event.Expert.ExpertEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors
import Service.Migrator.Applicator.ApplyEvent
import Service.Migrator.Applicator.Errors
import Service.Migrator.Applicator.Modifiers
import Service.Migrator.Applicator.Utils

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO KNOWLEDGE MODEL
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyEventToChapters e path km chUuid =
  hFoldl foldOneChapter (Right []) (km ^. chapters) $ \mChapters -> Right . Just $ km & chapters .~ mChapters
  where
    foldOneChapter :: Either AppError [Chapter] -> Chapter -> Either AppError [Chapter]
    foldOneChapter (Left error) _ = Left error
    foldOneChapter (Right chs) chapter =
      if chapter ^. uuid == chUuid
        then heApplyEventToChapter e path (Right chapter) $ \appliedChapter -> Right $ chs ++ [appliedChapter]
        else Right $ chs ++ [chapter]

passToChapters _ _ (Left error) = Left error
passToChapters e _ (Right Nothing) = errorEditNonExistingThing e
passToChapters e (_:pChUuid:restOfPath) (Right (Just km)) =
  applyEventToChapters e (pChUuid : restOfPath) km (pChUuid ^. uuid)
passToChapters e _ _ = errorEmptyPath e

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToKM AddKnowledgeModelEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right (Just _)) = Left . MigratorError $ _ERROR_MT_VALIDATION_APPLICATOR__KM_UNIQUENESS
  applyEventToKM e [] (Right Nothing) = Right . Just . createKM $ e
  applyEventToKM e path _ = errorPathShouldBeEmpty e path

instance ApplyEventToKM EditKnowledgeModelEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e [] (Right (Just km)) = Right . Just $ editKM e km
  applyEventToKM e path _ = errorPathShouldBeEmpty e path

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToKM AddChapterEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e _ (Right (Just km)) = Right . Just $ addChapter km (createChapter e)

instance ApplyEventToKM EditChapterEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e (_:restOfPath) (Right (Just km)) = applyEventToChapters e restOfPath km (e ^. chapterUuid)
  applyEventToKM e [] _ = errorEmptyPath e

instance ApplyEventToKM DeleteChapterEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e _ (Right (Just km)) = Right . Just $ deleteChapter km (e ^. chapterUuid)

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToKM AddQuestionEvent where
  applyEventToKM e path eKm = passToChapters e path eKm

instance ApplyEventToKM EditQuestionEvent where
  applyEventToKM e path eKm = passToChapters e path eKm

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
-- APPLY TO CHAPTER
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyEventToQuestions e path ch qUuid =
  hFoldl foldOneQuestion (Right []) (ch ^. questions) $ \mQuestions -> Right $ ch & questions .~ mQuestions
  where
    foldOneQuestion :: Either AppError [Question] -> Question -> Either AppError [Question]
    foldOneQuestion (Left error) _ = Left error
    foldOneQuestion (Right qs) question =
      if question ^. uuid == qUuid
        then heApplyEventToQuestion e path (Right question) $ \appliedQuestion -> Right $ qs ++ [appliedQuestion]
        else Right $ qs ++ [question]

passToQuestions _ _ (Left error) = Left error
passToQuestions e (_:pQUuid:restOfPath) (Right ch) = applyEventToQuestions e (pQUuid : restOfPath) ch (pQUuid ^. uuid)
passToQuestions e _ _ = errorEmptyPath e

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToChapter AddKnowledgeModelEvent where
  applyEventToChapter e _ _ = errorIllegalState e "AddKnowledgeModelEvent" "Chapter"

instance ApplyEventToChapter EditKnowledgeModelEvent where
  applyEventToChapter e _ _ = errorIllegalState e "EditKnowledgeModelEvent" "Chapter"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToChapter AddChapterEvent where
  applyEventToChapter e _ _ = errorIllegalState e "AddChapterEvent" "Chapter"

instance ApplyEventToChapter EditChapterEvent where
  applyEventToChapter _ _ (Left error) = Left error
  applyEventToChapter e [] (Right ch) = Right $ editChapter e ch
  applyEventToChapter e path (Right ch) = errorPathShouldBeEmpty e path

instance ApplyEventToChapter DeleteChapterEvent where
  applyEventToChapter e _ _ = errorIllegalState e "DeleteChapterEvent" "Chapter"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToChapter AddQuestionEvent where
  applyEventToChapter _ _ (Left error) = Left error
  applyEventToChapter e (pChUuid:[]) (Right ch) = heCreateQuestion e $ \question -> Right $ addQuestion ch question
  applyEventToChapter e path (Right ch) = passToQuestions e path (Right ch)

instance ApplyEventToChapter EditQuestionEvent where
  applyEventToChapter _ _ (Left error) = Left error
  applyEventToChapter e (_:[]) (Right ch) = applyEventToQuestions e [] ch (e ^. questionUuid)
  applyEventToChapter e path (Right ch) = passToQuestions e path (Right ch)

instance ApplyEventToChapter DeleteQuestionEvent where
  applyEventToChapter _ _ (Left error) = Left error
  applyEventToChapter e (pChUuid:[]) (Right ch) = Right $ deleteQuestion ch (e ^. questionUuid)
  applyEventToChapter e path (Right ch) = passToQuestions e path (Right ch)

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToChapter AddAnswerEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter EditAnswerEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter DeleteAnswerEvent where
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
-- APPLY TO QUESTION
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
passToAnswersAndAnswerItemTemplate e path eQ = passToAnswerItemTemplate e path $ passToAnswers e path eQ

applyEventToAnswers e path q ansUuid =
  case q ^. answers of
    Just as -> hFoldl foldOneAnswer (Right []) as $ \mAnswers -> Right $ q & answers .~ (Just mAnswers)
    Nothing -> Right q
  where
    foldOneAnswer :: Either AppError [Answer] -> Answer -> Either AppError [Answer]
    foldOneAnswer (Left error) _ = Left error
    foldOneAnswer (Right as) answer =
      if answer ^. uuid == ansUuid
        then heApplyEventToAnswer e path (Right answer) $ \appliedAnswer -> Right $ as ++ [appliedAnswer]
        else Right $ as ++ [answer]

passToAnswers _ _ (Left error) = Left error
passToAnswers e (_:pAnsUuid:restOfPath) (Right q) = applyEventToAnswers e (pAnsUuid : restOfPath) q (pAnsUuid ^. uuid)
passToAnswers e _ _ = errorEmptyPath e

-- --------------------------------------------
applyEventToAit e path ait qUuid =
  hFoldl foldOneQuestion (Right []) (ait ^. questions) $ \mQuestions -> Right $ ait & questions .~ mQuestions
  where
    foldOneQuestion :: Either AppError [Question] -> Question -> Either AppError [Question]
    foldOneQuestion (Left error) _ = Left error
    foldOneQuestion (Right qs) question =
      if question ^. uuid == qUuid
        then heApplyEventToQuestion e path (Right question) $ \appliedQuestion -> Right $ qs ++ [appliedQuestion]
        else Right $ qs ++ [question]

passToAnswerItemTemplate _ _ (Left error) = Left error
passToAnswerItemTemplate e (_:pQUuid:restOfPath) (Right q) =
  case q ^. answerItemTemplate of
    Just ait ->
      case modifyAit ait of
        Left error -> Left error
        Right modifiedAit -> Right $ q & answerItemTemplate .~ (Just modifiedAit)
    Nothing -> Right q
  where
    modifyAit :: AnswerItemTemplate -> Either AppError AnswerItemTemplate
    modifyAit ait = applyEventToAit e (pQUuid : restOfPath) ait (pQUuid ^. uuid)

-- --------------------------------------------
applyEventToExperts e path q expUuid =
  hFoldl foldOneExpert (Right []) (q ^. experts) $ \mExperts -> Right $ q & experts .~ mExperts
  where
    foldOneExpert :: Either AppError [Expert] -> Expert -> Either AppError [Expert]
    foldOneExpert (Left error) _ = Left error
    foldOneExpert (Right es) expert =
      if expert ^. uuid == expUuid
        then heApplyEventToExpert e path (Right expert) $ \appliedExpert -> Right $ es ++ [appliedExpert]
        else Right $ es ++ [expert]

-- --------------------------------------------
applyEventToReferences e path q refUuid =
  hFoldl foldOneReference (Right []) (q ^. references) $ \mReferences -> Right $ q & references .~ mReferences
  where
    foldOneReference :: Either AppError [Reference] -> Reference -> Either AppError [Reference]
    foldOneReference (Left error) _ = Left error
    foldOneReference (Right es) reference =
      if (getReferenceUuid reference) == refUuid
        then heApplyEventToReference e path (Right reference) $ \appliedReference -> Right $ es ++ [appliedReference]
        else Right $ es ++ [reference]

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToQuestion AddKnowledgeModelEvent where
  applyEventToQuestion e _ _ = errorIllegalState e "AddKnowledgeModelEvent" "Question"

instance ApplyEventToQuestion EditKnowledgeModelEvent where
  applyEventToQuestion e _ _ = errorIllegalState e "EditKnowledgeModelEvent" "Question"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToQuestion AddChapterEvent where
  applyEventToQuestion e _ _ = errorIllegalState e "AddChapterEvent" "Question"

instance ApplyEventToQuestion EditChapterEvent where
  applyEventToQuestion e _ _ = errorIllegalState e "EditChapterEvent" "Question"

instance ApplyEventToQuestion DeleteChapterEvent where
  applyEventToQuestion e _ _ = errorIllegalState e "DeleteChapterEvent" "Question"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToQuestion AddQuestionEvent where
  applyEventToQuestion _ _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) =
    heCreateQuestion e $ \aitQuestion -> Right $ addAitQuestion q aitQuestion
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

instance ApplyEventToQuestion EditQuestionEvent where
  applyEventToQuestion _ _ (Left error) = Left error
  applyEventToQuestion e [] (Right q) = Right $ editQuestion e q
  applyEventToQuestion e (pQUuid:[]) (Right q) =
    case q ^. answerItemTemplate of
      Just ait ->
        case modifyAit ait of
          Left error -> Left error
          Right modifiedAit -> Right $ q & answerItemTemplate .~ (Just modifiedAit)
      Nothing -> Right q
    where
      modifyAit :: AnswerItemTemplate -> Either AppError AnswerItemTemplate
      modifyAit ait = applyEventToAit e [] ait (e ^. questionUuid)
  applyEventToQuestion e path (Right q) = passToAnswersAndAnswerItemTemplate e path (Right q)

instance ApplyEventToQuestion DeleteQuestionEvent where
  applyEventToQuestion _ _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ deleteAitQuestion q (e ^. questionUuid)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToQuestion AddAnswerEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ addAnswer q (createAnswer e)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

instance ApplyEventToQuestion EditAnswerEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = applyEventToAnswers e [] q (e ^. answerUuid)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

instance ApplyEventToQuestion DeleteAnswerEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ deleteAnswer q (e ^. answerUuid)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToQuestion AddExpertEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ addExpert q (createExpert e)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

instance ApplyEventToQuestion EditExpertEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = applyEventToExperts e [] q (e ^. expertUuid)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

instance ApplyEventToQuestion DeleteExpertEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ deleteExpert q (e ^. expertUuid)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToQuestion AddReferenceEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ addReference q (createReference e)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

instance ApplyEventToQuestion EditReferenceEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = applyEventToReferences e [] q (getEventNodeUuid e)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

instance ApplyEventToQuestion DeleteReferenceEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ deleteReference q (getEventNodeUuid e)
  applyEventToQuestion e path eQ = passToAnswersAndAnswerItemTemplate e path eQ

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO ANSWER
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyEventToFollowUps e path answer qUuid =
  hFoldl foldOneFollowUps (Right []) (answer ^. followUps) $ \mFollowUps -> Right $ answer & followUps .~ mFollowUps
  where
    foldOneFollowUps :: Either AppError [Question] -> Question -> Either AppError [Question]
    foldOneFollowUps (Left error) _ = Left error
    foldOneFollowUps (Right qs) question =
      if question ^. uuid == qUuid
        then heApplyEventToQuestion e path (Right question) $ \appliedQuestion -> Right $ qs ++ [appliedQuestion]
        else Right $ qs ++ [question]

passToFollowUps _ _ (Left error) = Left error
passToFollowUps e (_:pQUuid:restOfPath) (Right answer) =
  applyEventToFollowUps e (pQUuid : restOfPath) answer (pQUuid ^. uuid)
passToFollowUps e _ _ = errorEmptyPath e

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToAnswer AddKnowledgeModelEvent where
  applyEventToAnswer e _ _ = errorIllegalState e "AddKnowledgeModelEvent" "Answer"

instance ApplyEventToAnswer EditKnowledgeModelEvent where
  applyEventToAnswer e _ _ = errorIllegalState e "EditKnowledgeModelEvent" "Answer"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToAnswer AddChapterEvent where
  applyEventToAnswer e _ _ = errorIllegalState e "AddChapterEvent" "Answer"

instance ApplyEventToAnswer EditChapterEvent where
  applyEventToAnswer e _ _ = errorIllegalState e "EditChapterEvent" "Answer"

instance ApplyEventToAnswer DeleteChapterEvent where
  applyEventToAnswer e _ _ = errorIllegalState e "DeleteChapterEvent" "Answer"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToAnswer AddQuestionEvent where
  applyEventToAnswer _ _ (Left error) = Left error
  applyEventToAnswer e (ansUuid:[]) (Right ans) = heCreateQuestion e $ \question -> Right $ addFuQuestion ans question
  applyEventToAnswer e path eAns = passToFollowUps e path eAns

instance ApplyEventToAnswer EditQuestionEvent where
  applyEventToAnswer _ _ (Left error) = Left error
  applyEventToAnswer e (ansUuid:[]) (Right ans) = applyEventToFollowUps e [] ans (e ^. questionUuid)
  applyEventToAnswer e path eAns = passToFollowUps e path eAns

instance ApplyEventToAnswer DeleteQuestionEvent where
  applyEventToAnswer _ _ (Left error) = Left error
  applyEventToAnswer e (ansUuid:[]) (Right ans) = Right $ deleteFuQuestion ans (e ^. questionUuid)
  applyEventToAnswer e path eAns = passToFollowUps e path eAns

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToAnswer AddAnswerEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer EditAnswerEvent where
  applyEventToAnswer _ _ (Left error) = Left error
  applyEventToAnswer e [] (Right ans) = Right $ editAnswer e ans
  applyEventToAnswer e path eAns = passToFollowUps e path eAns

instance ApplyEventToAnswer DeleteAnswerEvent where
  applyEventToAnswer = passToFollowUps

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToAnswer AddExpertEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer EditExpertEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer DeleteExpertEvent where
  applyEventToAnswer = passToFollowUps

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToAnswer AddReferenceEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer EditReferenceEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer DeleteReferenceEvent where
  applyEventToAnswer = passToFollowUps

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO EXPERT
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToExpert AddKnowledgeModelEvent where
  applyEventToExpert e _ _ = errorIllegalState e "AddKnowledgeModelEvent" "Expert"

instance ApplyEventToExpert EditKnowledgeModelEvent where
  applyEventToExpert e _ _ = errorIllegalState e "EditKnowledgeModelEvent" "Expert"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToExpert AddChapterEvent where
  applyEventToExpert e _ _ = errorIllegalState e "AddChapterEvent" "Expert"

instance ApplyEventToExpert EditChapterEvent where
  applyEventToExpert e _ _ = errorIllegalState e "EditChapterEvent" "Expert"

instance ApplyEventToExpert DeleteChapterEvent where
  applyEventToExpert e _ _ = errorIllegalState e "DeleteChapterEvent" "Expert"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToExpert AddQuestionEvent where
  applyEventToExpert e _ _ = errorIllegalState e "AddQuestionEvent" "Expert"

instance ApplyEventToExpert EditQuestionEvent where
  applyEventToExpert e _ _ = errorIllegalState e "EditQuestionEvent" "Expert"

instance ApplyEventToExpert DeleteQuestionEvent where
  applyEventToExpert e _ _ = errorIllegalState e "DeleteQuestionEvent" "Expert"

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToExpert AddAnswerEvent where
  applyEventToExpert e _ _ = errorIllegalState e "AddAnswerEvent" "Expert"

instance ApplyEventToExpert EditAnswerEvent where
  applyEventToExpert e _ _ = errorIllegalState e "EditAnswerEvent" "Expert"

instance ApplyEventToExpert DeleteAnswerEvent where
  applyEventToExpert e _ _ = errorIllegalState e "DeleteAnswerEvent" "Expert"

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToExpert AddExpertEvent where
  applyEventToExpert e _ _ = errorIllegalState e "AddExpertEvent" "Expert"

instance ApplyEventToExpert EditExpertEvent where
  applyEventToExpert _ _ (Left error) = Left error
  applyEventToExpert e [] (Right exp) = Right $ editExpert e exp
  applyEventToExpert e path _ = errorPathShouldBeEmpty e path

instance ApplyEventToExpert DeleteExpertEvent where
  applyEventToExpert e _ _ = errorIllegalState e "DeleteExpertEvent" "Expert"

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToExpert AddReferenceEvent where
  applyEventToExpert e _ _ = errorIllegalState e "AddReferenceEvent" "Expert"

instance ApplyEventToExpert EditReferenceEvent where
  applyEventToExpert e _ _ = errorIllegalState e "EditReferenceEvent" "Expert"

instance ApplyEventToExpert DeleteReferenceEvent where
  applyEventToExpert e _ _ = errorIllegalState e "DeleteReferenceEvent" "Expert"

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO REFERENCE
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToReference AddKnowledgeModelEvent where
  applyEventToReference e _ _ = errorIllegalState e "AddKnowledgeModelEvent" "Reference"

instance ApplyEventToReference EditKnowledgeModelEvent where
  applyEventToReference e _ _ = errorIllegalState e "EditKnowledgeModelEvent" "Reference"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToReference AddChapterEvent where
  applyEventToReference e _ _ = errorIllegalState e "AddChapterEvent" "Reference"

instance ApplyEventToReference EditChapterEvent where
  applyEventToReference e _ _ = errorIllegalState e "EditChapterEvent" "Reference"

instance ApplyEventToReference DeleteChapterEvent where
  applyEventToReference e _ _ = errorIllegalState e "DeleteChapterEvent" "Reference"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToReference AddQuestionEvent where
  applyEventToReference e _ _ = errorIllegalState e "AddQuestionEvent" "Reference"

instance ApplyEventToReference EditQuestionEvent where
  applyEventToReference e _ _ = errorIllegalState e "EditQuestionEvent" "Reference"

instance ApplyEventToReference DeleteQuestionEvent where
  applyEventToReference e _ _ = errorIllegalState e "DeleteQuestionEvent" "Reference"

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToReference AddAnswerEvent where
  applyEventToReference e _ _ = errorIllegalState e "AddAnswerEvent" "Reference"

instance ApplyEventToReference EditAnswerEvent where
  applyEventToReference e _ _ = errorIllegalState e "EditAnswerEvent" "Reference"

instance ApplyEventToReference DeleteAnswerEvent where
  applyEventToReference e _ _ = errorIllegalState e "DeleteAnswerEvent" "Reference"

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToReference AddExpertEvent where
  applyEventToReference e _ _ = errorIllegalState e "AddExpertEvent" "Reference"

instance ApplyEventToReference EditExpertEvent where
  applyEventToReference e _ _ = errorIllegalState e "EditExpertEvent" "Reference"

instance ApplyEventToReference DeleteExpertEvent where
  applyEventToReference e _ _ = errorIllegalState e "DeleteExpertEvent" "Reference"

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToReference AddReferenceEvent where
  applyEventToReference e _ _ = errorIllegalState e "AddReferenceEvent" "Reference"

instance ApplyEventToReference EditReferenceEvent where
  applyEventToReference _ _ (Left error) = Left error
  applyEventToReference e [] (Right ref) = Right $ editReference e ref
  applyEventToReference e path _ = errorPathShouldBeEmpty e path

instance ApplyEventToReference DeleteReferenceEvent where
  applyEventToReference e _ _ = errorIllegalState e "DeleteReferenceEvent" "Reference"

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- HELPERS
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
heCreateQuestion e callback =
  getAnswerItemTemplate $ \maybeAit -> getAnswers $ \maybeAns -> callback $ createQuestion e maybeAit maybeAns
  where
    getAnswerItemTemplate callback =
      case e ^. qType of
        QuestionTypeList ->
          case e ^. answerItemTemplatePlain of
            Just ait ->
              callback . Just $
              AnswerItemTemplate {_answerItemTemplateTitle = ait ^. title, _answerItemTemplateQuestions = []}
            Nothing -> Left . MigratorError $ _ERROR_MT_APPLICATOR__Q_TYPE_LIST_REQUIRES_AIT
        _ -> callback Nothing
    getAnswers callback =
      case e ^. qType of
        QuestionTypeOptions -> callback . Just $ []
        _ -> callback Nothing
