module Service.Migrator.Applicator where

import Control.Lens

import Common.Error
import Common.Localization
import LensesConfig
import Model.Common
import Model.Event.Answer.AnswerEvent
import Model.Event.Answer.AnswerEventSameUuid ()
import Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEvent
import Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEventSameUuid ()
import Model.Event.Chapter.ChapterEvent
import Model.Event.Chapter.ChapterEventSameUuid ()
import Model.Event.Event
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.Expert.ExpertEventSameUuid ()
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.Event.FollowUpQuestion.FollowUpQuestionEventSameUuid ()
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.KnowledgeModel.KnowledgeModelEventSameUuid ()
import Model.Event.Question.QuestionEvent
import Model.Event.Question.QuestionEventSameUuid ()
import Model.Event.Reference.ReferenceEvent
import Model.Event.Reference.ReferenceEventSameUuid ()
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors

runApplicator :: Maybe KnowledgeModel -> [Event] -> Either AppError KnowledgeModel
runApplicator mKM events =
  case foldl foldEvent (Right mKM) events of
    Left error -> Left error
    Right Nothing -> Left . MigratorError $ _ERROR_MT_APPLICATOR__UNSPECIFIED_ERROR
    Right (Just km) -> Right km

foldEvent :: Either AppError (Maybe KnowledgeModel) -> Event -> Either AppError (Maybe KnowledgeModel)
foldEvent emKM (AddKnowledgeModelEvent' e) = applyEventToKM e emKM
foldEvent emKM (EditKnowledgeModelEvent' e) = applyEventToKM e emKM
foldEvent emKM (AddChapterEvent' e) = applyEventToKM e emKM
foldEvent emKM (EditChapterEvent' e) = applyEventToKM e emKM
foldEvent emKM (DeleteChapterEvent' e) = applyEventToKM e emKM
foldEvent emKM (AddQuestionEvent' e) = applyEventToKM e emKM
foldEvent emKM (EditQuestionEvent' e) = applyEventToKM e emKM
foldEvent emKM (DeleteQuestionEvent' e) = applyEventToKM e emKM
foldEvent emKM (AddAnswerEvent' e) = applyEventToKM e emKM
foldEvent emKM (EditAnswerEvent' e) = applyEventToKM e emKM
foldEvent emKM (DeleteAnswerEvent' e) = applyEventToKM e emKM
foldEvent emKM (AddExpertEvent' e) = applyEventToKM e emKM
foldEvent emKM (EditExpertEvent' e) = applyEventToKM e emKM
foldEvent emKM (DeleteExpertEvent' e) = applyEventToKM e emKM
foldEvent emKM (AddReferenceEvent' e) = applyEventToKM e emKM
foldEvent emKM (EditReferenceEvent' e) = applyEventToKM e emKM
foldEvent emKM (DeleteReferenceEvent' e) = applyEventToKM e emKM
foldEvent emKM (AddFollowUpQuestionEvent' e) = applyEventToKM e emKM
foldEvent emKM (EditFollowUpQuestionEvent' e) = applyEventToKM e emKM
foldEvent emKM (DeleteFollowUpQuestionEvent' e) = applyEventToKM e emKM
foldEvent emKM (AddAnswerItemTemplateQuestionEvent' e) = applyEventToKM e emKM
foldEvent emKM (EditAnswerItemTemplateQuestionEvent' e) = applyEventToKM e emKM
foldEvent emKM (DeleteAnswerItemTemplateQuestionEvent' e) = applyEventToKM e emKM

applyValue (ChangedValue val) ch setter = ch & setter .~ val
applyValue NothingChanged ch setter = ch

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToKM e where
  applyEventToKM :: e -> Either AppError (Maybe KnowledgeModel) -> Either AppError (Maybe KnowledgeModel)

passToChapters _ (Left error) = Left error
passToChapters e (Right Nothing) = Left . MigratorError $ _ERROR_MT_APPLICATOR__CREATE_KM_AT_FIRST
passToChapters e (Right (Just km)) =
  case eModifiedChapters of
    Left error -> Left error
    Right modifiedChapters -> Right . Just $ km & chapters .~ modifiedChapters
  where
    eModifiedChapters = foldl foldOneChapter (Right []) (km ^. chapters)
    foldOneChapter :: Either AppError [Chapter] -> Chapter -> Either AppError [Chapter]
    foldOneChapter (Left error) _ = Left error
    foldOneChapter (Right kmChapters) chapter =
      case applyEventToChapter e (Right chapter) of
        Left error -> Left error
        Right appliedChapter -> Right $ kmChapters ++ [appliedChapter]

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToKM AddKnowledgeModelEvent where
  applyEventToKM _ (Left error) = Left error
  applyEventToKM e (Right (Just _)) = Left . MigratorError $ _ERROR_MT_VALIDATION_APPLICATOR__KM_UNIQUENESS
  applyEventToKM e (Right Nothing) =
    Right . Just $
    KnowledgeModel {_knowledgeModelUuid = e ^. kmUuid, _knowledgeModelName = e ^. name, _knowledgeModelChapters = []}

instance ApplyEventToKM EditKnowledgeModelEvent where
  applyEventToKM _ (Left error) = Left error
  applyEventToKM e (Right Nothing) = Left . MigratorError $ _ERROR_MT_APPLICATOR__CREATE_KM_AT_FIRST
  applyEventToKM e (Right (Just km)) = Right . Just . applyChapterIds . applyName $ km
    where
      applyName km = applyValue (e ^. name) km name
      applyChapterIds km = applyValue (e ^. chapterIds) km kmChangeChapterIdsOrder

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToKM AddChapterEvent where
  applyEventToKM _ (Left error) = Left error
  applyEventToKM e (Right Nothing) = Left . MigratorError $ _ERROR_MT_APPLICATOR__CREATE_KM_AT_FIRST
  applyEventToKM e (Right (Just km)) = Right . Just $ km & chapters .~ modifiedChapters
    where
      modifiedChapters = km ^. chapters ++ [newChapter]
      newChapter =
        Chapter
        {_chapterUuid = e ^. chapterUuid, _chapterTitle = e ^. title, _chapterText = e ^. text, _chapterQuestions = []}

instance ApplyEventToKM EditChapterEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM DeleteChapterEvent where
  applyEventToKM _ (Left error) = Left error
  applyEventToKM e (Right Nothing) = Left . MigratorError $ _ERROR_MT_APPLICATOR__CREATE_KM_AT_FIRST
  applyEventToKM e (Right (Just km)) =
    if equalsUuid e km
      then Right . Just $ km & chapters .~ modifiedChapters
      else Right . Just $ km
    where
      modifiedChapters = filter (not . equalsUuid e) (km ^. chapters)

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

-- -----------------------------------
-- ANSWER-ITEM-TEMPLATE-QUESTIONS ----
-- -----------------------------------
instance ApplyEventToKM AddAnswerItemTemplateQuestionEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM EditAnswerItemTemplateQuestionEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM DeleteAnswerItemTemplateQuestionEvent where
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
  applyEventToChapter :: e -> Either AppError Chapter -> Either AppError Chapter

passToQuestions _ (Left error) = Left error
passToQuestions e (Right ch) =
  case eModifiedQuestions of
    Left error -> Left error
    Right modifiedQuestions -> Right $ ch & questions .~ modifiedQuestions
  where
    eModifiedQuestions = foldl foldOneQuestion (Right []) (ch ^. questions)
    foldOneQuestion :: Either AppError [Question] -> Question -> Either AppError [Question]
    foldOneQuestion (Left error) _ = Left error
    foldOneQuestion (Right questions) question =
      case applyEventToQuestion e (Right question) of
        Left error -> Left error
        Right appliedQuestion -> Right $ questions ++ [appliedQuestion]

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToChapter AddKnowledgeModelEvent where
  applyEventToChapter _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddKnowledgeModelEvent" "Chapter"

instance ApplyEventToChapter EditKnowledgeModelEvent where
  applyEventToChapter _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditKnowledgeModelEvent" "Chapter"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToChapter AddChapterEvent where
  applyEventToChapter _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddChapterEvent" "Chapter"

instance ApplyEventToChapter EditChapterEvent where
  applyEventToChapter _ (Left error) = Left error
  applyEventToChapter e (Right ch) =
    if equalsUuid e ch
      then Right . applyQuestionIds . applyText . applyTitle $ ch
      else Right ch
    where
      applyTitle ch = applyValue (e ^. title) ch title
      applyText ch = applyValue (e ^. text) ch text
      applyQuestionIds ch = applyValue (e ^. questionIds) ch chChangeQuestionIdsOrder

instance ApplyEventToChapter DeleteChapterEvent where
  applyEventToChapter _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteChapterEvent" "Chapter"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToChapter AddQuestionEvent where
  applyEventToChapter _ (Left error) = Left error
  applyEventToChapter e (Right ch) =
    if equalsUuid e ch
      then getModifiedQuestions $ \modifiedQuestions -> Right $ ch & questions .~ modifiedQuestions
      else Right ch
    where
      getModifiedQuestions callback = getNewQuestion $ \newQuestion -> callback $ ch ^. questions ++ [newQuestion]
      getNewQuestion callback =
        getAnswerItemTemplate $ \maybeAit ->
          getAnswers $ \maybeAnswers ->
            callback
              Question
              { _questionUuid = e ^. questionUuid
              , _questionShortUuid = e ^. shortQuestionUuid
              , _questionQType = e ^. qType
              , _questionTitle = e ^. title
              , _questionText = e ^. text
              , _questionAnswerItemTemplate = maybeAit
              , _questionAnswers = maybeAnswers
              , _questionReferences = []
              , _questionExperts = []
              }
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

instance ApplyEventToChapter EditQuestionEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter DeleteQuestionEvent where
  applyEventToChapter _ (Left error) = Left error
  applyEventToChapter e (Right ch) =
    if equalsUuid e ch
      then Right $ ch & questions .~ modifiedQuestions
      else passToQuestions e (Right ch)
    where
      modifiedQuestions = filter (not . equalsUuid e) (ch ^. questions)

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

-- -----------------------------------
-- ANSWER-ITEM-TEMPLATE-QUESTIONS ----
-- -----------------------------------
instance ApplyEventToChapter AddAnswerItemTemplateQuestionEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter EditAnswerItemTemplateQuestionEvent where
  applyEventToChapter = passToQuestions

instance ApplyEventToChapter DeleteAnswerItemTemplateQuestionEvent where
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
  applyEventToQuestion :: e -> Either AppError Question -> Either AppError Question

passToAnswersAndAnswerItemTemplate e = passToAnswerItemTemplate e . passToAnswers e

passToAnswers _ (Left error) = Left error
passToAnswers e (Right q) =
  case q ^. answers of
    Just as ->
      case eModifiedAnswers as of
        Left error -> Left error
        Right modifiedAnswers -> Right $ q & answers .~ Just modifiedAnswers
    Nothing -> Right q
  where
    eModifiedAnswers = foldl foldOneAnswer (Right [])
    foldOneAnswer :: Either AppError [Answer] -> Answer -> Either AppError [Answer]
    foldOneAnswer (Left error) _ = Left error
    foldOneAnswer (Right qAnswers) answer =
      case applyEventToAnswer e (Right answer) of
        Left error -> Left error
        Right appliedAnswers -> Right $ qAnswers ++ [appliedAnswers]

passToAnswerItemTemplate _ (Left error) = Left error
passToAnswerItemTemplate e (Right q) =
  case q ^. answerItemTemplate of
    Just ait ->
      case eModifiedAnswerItemTemplate ait of
        Left error -> Left error
        Right modifiedAnswerItemTemplate -> Right $ q & answerItemTemplate .~ (Just modifiedAnswerItemTemplate)
    Nothing -> Right q
  where
    eModifiedAnswerItemTemplate :: AnswerItemTemplate -> Either AppError AnswerItemTemplate
    eModifiedAnswerItemTemplate ait =
      case eModifiedQuestions ait of
        Right modifiedQuestions -> Right $ ait & questions .~ modifiedQuestions
        Left error -> Left error
    eModifiedQuestions :: AnswerItemTemplate -> Either AppError [Question]
    eModifiedQuestions ait = foldl foldOneQuestion (Right []) (ait ^. questions)
    foldOneQuestion :: Either AppError [Question] -> Question -> Either AppError [Question]
    foldOneQuestion (Left error) _ = Left error
    foldOneQuestion (Right aitQuestions) question =
      case applyEventToQuestion e (Right question) of
        Left error -> Left error
        Right appliedQuestions -> Right $ aitQuestions ++ [appliedQuestions]

passToExperts _ (Left error) = Left error
passToExperts e (Right q) =
  case eModifiedExperts of
    Left error -> Left error
    Right modifiedExperts -> Right $ q & experts .~ modifiedExperts
  where
    eModifiedExperts = foldl foldOneExpert (Right []) (q ^. experts)
    foldOneExpert :: Either AppError [Expert] -> Expert -> Either AppError [Expert]
    foldOneExpert (Left error) _ = Left error
    foldOneExpert (Right qExperts) expert =
      case applyEventToExpert e (Right expert) of
        Left error -> Left error
        Right appliedExpert -> Right $ qExperts ++ [appliedExpert]

passToReferences _ (Left error) = Left error
passToReferences e (Right q) =
  case eModifiedReferences of
    Left error -> Left error
    Right modifiedReferences -> Right $ q & references .~ modifiedReferences
  where
    eModifiedReferences = foldl foldOneReference (Right []) (q ^. references)
    foldOneReference :: Either AppError [Reference] -> Reference -> Either AppError [Reference]
    foldOneReference (Left error) _ = Left error
    foldOneReference (Right qReferences) reference =
      case applyEventToReference e (Right reference) of
        Left error -> Left error
        Right appliedReference -> Right $ qReferences ++ [appliedReference]

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToQuestion AddKnowledgeModelEvent where
  applyEventToQuestion _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddKnowledgeModelEvent" "Question"

instance ApplyEventToQuestion EditKnowledgeModelEvent where
  applyEventToQuestion _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditKnowledgeModelEvent" "Question"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToQuestion AddChapterEvent where
  applyEventToQuestion _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddChapterEvent" "Question"

instance ApplyEventToQuestion EditChapterEvent where
  applyEventToQuestion _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditChapterEvent" "Question"

instance ApplyEventToQuestion DeleteChapterEvent where
  applyEventToQuestion _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteChapterEvent" "Question"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToQuestion AddQuestionEvent where
  applyEventToQuestion = passToAnswersAndAnswerItemTemplate

instance ApplyEventToQuestion EditQuestionEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right .
           applyReferenceIds .
           applyExpertIds .
           applyAnwerIds . applyAnswerItemTemplate . applyText . applyTitle . applyType . applyShortUuid $
           q
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      applyShortUuid q = applyValue (e ^. shortQuestionUuid) q shortUuid
      applyType q = applyValue (e ^. qType) q qType
      applyTitle q = applyValue (e ^. title) q title
      applyText q = applyValue (e ^. text) q text
      applyAnswerItemTemplate q = applyValue (e ^. answerItemTemplatePlainWithIds) q aitAnswerItemTemplatePlainWithIds
      applyAnwerIds q = applyValue (e ^. answerIds) q qChangeAnwerIdsOrder
      applyExpertIds q = applyValue (e ^. expertIds) q qChangeExpertIdsOrder
      applyReferenceIds q = applyValue (e ^. referenceIds) q qChangeReferenceIdsOrder

instance ApplyEventToQuestion DeleteQuestionEvent where
  applyEventToQuestion = passToAnswersAndAnswerItemTemplate

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToQuestion AddAnswerEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & answers .~ (Just modifiedAnswers)
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      modifiedAnswers =
        case q ^. answers of
          Just as -> as ++ [newAnswer]
          Nothing -> [newAnswer]
      newAnswer =
        Answer
        {_answerUuid = e ^. answerUuid, _answerLabel = e ^. label, _answerAdvice = e ^. advice, _answerFollowUps = []}

instance ApplyEventToQuestion EditAnswerEvent where
  applyEventToQuestion = passToAnswersAndAnswerItemTemplate

instance ApplyEventToQuestion DeleteAnswerEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & answers .~ modifiedAnswers
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      modifiedAnswers =
        case (q ^. answers) of
          Just as -> Just $ filter (not . equalsUuid e) as
          Nothing -> Nothing

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToQuestion AddFollowUpQuestionEvent where
  applyEventToQuestion = passToAnswersAndAnswerItemTemplate

instance ApplyEventToQuestion EditFollowUpQuestionEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right .
           applyReferenceIds .
           applyExpertIds .
           applyAnwerIds . applyAnswerItemTemplate . applyText . applyTitle . applyType . applyShortQuestionId $
           q
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      applyShortQuestionId q = applyValue (e ^. shortQuestionUuid) q shortUuid
      applyType q = applyValue (e ^. qType) q qType
      applyTitle q = applyValue (e ^. title) q title
      applyText q = applyValue (e ^. text) q text
      applyAnswerItemTemplate q = applyValue (e ^. answerItemTemplatePlainWithIds) q aitAnswerItemTemplatePlainWithIds
      applyAnwerIds q = applyValue (e ^. answerIds) q qChangeAnwerIdsOrder
      applyExpertIds q = applyValue (e ^. expertIds) q qChangeExpertIdsOrder
      applyReferenceIds q = applyValue (e ^. referenceIds) q qChangeReferenceIdsOrder

instance ApplyEventToQuestion DeleteFollowUpQuestionEvent where
  applyEventToQuestion = passToAnswersAndAnswerItemTemplate

-- -----------------------------------
-- ANSWER-ITEM-TEMPLATE-QUESTIONS ----
-- -----------------------------------
instance ApplyEventToQuestion AddAnswerItemTemplateQuestionEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if (e ^. parentQuestionUuid) == (q ^. uuid)
      then unwrapAnswerItemTemplate $ \oldAit ->
             getModifiedAnswerItemTemplate oldAit $ \modifiedAit -> Right $ q & answerItemTemplate .~ modifiedAit
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      getModifiedAnswerItemTemplate oldAit callback =
        getModifiedAitQuestions oldAit $ \modifiedAitQuestions ->
          callback . Just $ oldAit & questions .~ modifiedAitQuestions
      getModifiedAitQuestions oldAit callback =
        getNewQuestion $ \newQuestion -> callback $ (oldAit ^. questions) ++ [newQuestion]
      unwrapAnswerItemTemplate callback =
        case q ^. answerItemTemplate of
          Nothing -> Left . MigratorError $ _ERROR_MT_APPLICATOR__YOU_CANT_ADD_QUESTION_TO_NON_EXISTING_AIT
          Just ait -> callback ait
      getNewQuestion callback =
        getAnswerItemTemplate $ \maybeAit ->
          getAnswers $ \maybeAnswers ->
            callback
              Question
              { _questionUuid = e ^. questionUuid
              , _questionShortUuid = e ^. shortQuestionUuid
              , _questionQType = e ^. qType
              , _questionTitle = e ^. title
              , _questionText = e ^. text
              , _questionAnswerItemTemplate = maybeAit
              , _questionAnswers = maybeAnswers
              , _questionReferences = []
              , _questionExperts = []
              }
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

instance ApplyEventToQuestion EditAnswerItemTemplateQuestionEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right .
           applyReferenceIds .
           applyExpertIds .
           applyAnwerIds . applyAnswerItemTemplate . applyText . applyTitle . applyType . applyShortUuid $
           q
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      applyShortUuid q = applyValue (e ^. shortQuestionUuid) q shortUuid
      applyType q = applyValue (e ^. qType) q qType
      applyTitle q = applyValue (e ^. title) q title
      applyText q = applyValue (e ^. text) q text
      applyAnswerItemTemplate q = applyValue (e ^. answerItemTemplatePlainWithIds) q aitAnswerItemTemplatePlainWithIds
      applyAnwerIds q = applyValue (e ^. answerIds) q qChangeAnwerIdsOrder
      applyExpertIds q = applyValue (e ^. expertIds) q qChangeExpertIdsOrder
      applyReferenceIds q = applyValue (e ^. referenceIds) q qChangeReferenceIdsOrder

instance ApplyEventToQuestion DeleteAnswerItemTemplateQuestionEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if (e ^. parentQuestionUuid) == (q ^. uuid)
      then unwrapAnswerItemTemplate $ \oldAit ->
             getModifiedAnswerItemTemplate oldAit $ \modifiedAit -> Right $ q & answerItemTemplate .~ modifiedAit
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      getModifiedAnswerItemTemplate oldAit callback =
        getModifiedAitQuestions oldAit $ \modifiedAitQuestions ->
          callback . Just $ oldAit & questions .~ modifiedAitQuestions
      getModifiedAitQuestions oldAit callback = callback $ filter (not . equalsUuid e) (oldAit ^. questions)
      unwrapAnswerItemTemplate callback =
        case q ^. answerItemTemplate of
          Nothing -> Left . MigratorError $ _ERROR_MT_APPLICATOR__YOU_CANT_ADD_QUESTION_TO_NON_EXISTING_AIT
          Just ait -> callback ait

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToQuestion AddExpertEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & experts .~ modifiedExperts
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      modifiedExperts = q ^. experts ++ [newExpert]
      newExpert = Expert {_expertUuid = e ^. expertUuid, _expertName = e ^. name, _expertEmail = e ^. email}

instance ApplyEventToQuestion EditExpertEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    passToReferences e . passToExperts e . passToAnswersAndAnswerItemTemplate e . Right $ q

instance ApplyEventToQuestion DeleteExpertEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & experts .~ modifiedExperts
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      modifiedExperts = filter (not . equalsUuid e) (q ^. experts)

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToQuestion AddReferenceEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & references .~ modifiedReferences
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      modifiedReferences = q ^. references ++ [newReference]
      newReference = Reference {_referenceUuid = e ^. referenceUuid, _referenceChapter = e ^. chapter}

instance ApplyEventToQuestion EditReferenceEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    passToReferences e . passToExperts e . passToAnswersAndAnswerItemTemplate e . Right $ q

instance ApplyEventToQuestion DeleteReferenceEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & references .~ modifiedReferences
      else passToAnswersAndAnswerItemTemplate e (Right q)
    where
      modifiedReferences = filter (not . equalsUuid e) (q ^. references)

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToAnswer e where
  applyEventToAnswer :: e -> Either AppError Answer -> Either AppError Answer

passToFollowUps _ (Left error) = Left error
passToFollowUps e (Right ans) =
  case eModifiedFollowUps of
    Left error -> Left error
    Right modifiedFollowUps -> Right $ ans & followUps .~ modifiedFollowUps
  where
    eModifiedFollowUps = foldl foldOneFollowUps (Right []) (ans ^. followUps)
    foldOneFollowUps :: Either AppError [Question] -> Question -> Either AppError [Question]
    foldOneFollowUps (Left error) _ = Left error
    foldOneFollowUps (Right answers) answer =
      case applyEventToQuestion e (Right answer) of
        Left error -> Left error
        Right appliedAnswer -> Right $ answers ++ [appliedAnswer]

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToAnswer AddKnowledgeModelEvent where
  applyEventToAnswer _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddKnowledgeModelEvent" "Answer"

instance ApplyEventToAnswer EditKnowledgeModelEvent where
  applyEventToAnswer _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditKnowledgeModelEvent" "Answer"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToAnswer AddChapterEvent where
  applyEventToAnswer _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddChapterEvent" "Answer"

instance ApplyEventToAnswer EditChapterEvent where
  applyEventToAnswer _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditChapterEvent" "Answer"

instance ApplyEventToAnswer DeleteChapterEvent where
  applyEventToAnswer _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteChapterEvent" "Answer"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToAnswer AddQuestionEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer EditQuestionEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer DeleteQuestionEvent where
  applyEventToAnswer = passToFollowUps

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToAnswer AddAnswerEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer EditAnswerEvent where
  applyEventToAnswer e (Left error) = Left error
  applyEventToAnswer e (Right ans) =
    if equalsUuid e ans
      then Right $ applyFollowUps . applyAdvice . applyLabel $ ans
      else passToFollowUps e (Right ans)
    where
      applyLabel ans = applyValue (e ^. label) ans label
      applyAdvice ans = applyValue (e ^. advice) ans advice
      applyFollowUps ans = applyValue (e ^. followUpIds) ans ansChangeFollowUpIdsOrder

instance ApplyEventToAnswer DeleteAnswerEvent where
  applyEventToAnswer = passToFollowUps

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToAnswer AddFollowUpQuestionEvent where
  applyEventToAnswer e (Left error) = Left error
  applyEventToAnswer e (Right ans) =
    if equalsUuid e ans
      then getModifiedFollowUps $ \modifiedFollowUps -> Right $ ans & followUps .~ modifiedFollowUps
      else passToFollowUps e (Right ans)
    where
      getModifiedFollowUps callback = getNewFollowUp $ \newFollowUp -> callback $ ans ^. followUps ++ [newFollowUp]
      getNewFollowUp callback =
        getAnswerItemTemplate $ \maybeAit ->
          getAnswers $ \maybeAnswers ->
            callback
              Question
              { _questionUuid = e ^. questionUuid
              , _questionShortUuid = e ^. shortQuestionUuid
              , _questionQType = e ^. qType
              , _questionTitle = e ^. title
              , _questionText = e ^. text
              , _questionAnswerItemTemplate = maybeAit
              , _questionAnswers = maybeAnswers
              , _questionReferences = []
              , _questionExperts = []
              }
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

instance ApplyEventToAnswer EditFollowUpQuestionEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer DeleteFollowUpQuestionEvent where
  applyEventToAnswer e (Left error) = Left error
  applyEventToAnswer e (Right ans) =
    if equalsUuid e ans
      then Right $ ans & followUps .~ modifiedFollowUps
      else passToFollowUps e (Right ans)
    where
      modifiedFollowUps = filter (not . equalsUuid e) (ans ^. followUps)

-- -----------------------------------
-- ANSWER-ITEM-TEMPLATE-QUESTIONS ----
-- -----------------------------------
instance ApplyEventToAnswer AddAnswerItemTemplateQuestionEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer EditAnswerItemTemplateQuestionEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer DeleteAnswerItemTemplateQuestionEvent where
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
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToExpert e where
  applyEventToExpert :: e -> Either AppError Expert -> Either AppError Expert

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToExpert AddKnowledgeModelEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddKnowledgeModelEvent" "Expert"

instance ApplyEventToExpert EditKnowledgeModelEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditKnowledgeModelEvent" "Expert"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToExpert AddChapterEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddChapterEvent" "Expert"

instance ApplyEventToExpert EditChapterEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditChapterEvent" "Expert"

instance ApplyEventToExpert DeleteChapterEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteChapterEvent" "Expert"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToExpert AddQuestionEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddQuestionEvent" "Expert"

instance ApplyEventToExpert EditQuestionEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditQuestionEvent" "Expert"

instance ApplyEventToExpert DeleteQuestionEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteQuestionEvent" "Expert"

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToExpert AddAnswerEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddAnswerEvent" "Expert"

instance ApplyEventToExpert EditAnswerEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditAnswerEvent" "Expert"

instance ApplyEventToExpert DeleteAnswerEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteAnswerEvent" "Expert"

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToExpert AddFollowUpQuestionEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddFollowUpQuestionEvent" "Expert"

instance ApplyEventToExpert EditFollowUpQuestionEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditFollowUpQuestionEvent" "Expert"

instance ApplyEventToExpert DeleteFollowUpQuestionEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteFollowUpQuestionEvent" "Expert"

-- -----------------------------------
-- ANSWER-ITEM-TEMPLATE-QUESTIONS ----
-- -----------------------------------
instance ApplyEventToExpert AddAnswerItemTemplateQuestionEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $
    _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddAnswerItemTemplateQuestionEvent" "Expert"

instance ApplyEventToExpert EditAnswerItemTemplateQuestionEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $
    _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditAnswerItemTemplateQuestionEvent" "Expert"

instance ApplyEventToExpert DeleteAnswerItemTemplateQuestionEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $
    _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteAnswerItemTemplateQuestionEvent" "Expert"

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToExpert AddExpertEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddExpertEvent" "Expert"

instance ApplyEventToExpert EditExpertEvent where
  applyEventToExpert e (Left error) = Left error
  applyEventToExpert e (Right exp) =
    if equalsUuid e exp
      then Right $ applyEmail . applyName $ exp
      else Right exp
    where
      applyName exp = applyValue (e ^. name) exp name
      applyEmail exp = applyValue (e ^. email) exp email

instance ApplyEventToExpert DeleteExpertEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteExpertEvent" "Expert"

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToExpert AddReferenceEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddReferenceEvent" "Expert"

instance ApplyEventToExpert EditReferenceEvent where
  applyEventToExpert e exp = exp

instance ApplyEventToExpert DeleteReferenceEvent where
  applyEventToExpert _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteReferenceEvent" "Expert"

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToReference e where
  applyEventToReference :: e -> Either AppError Reference -> Either AppError Reference

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToReference AddKnowledgeModelEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddKnowledgeModelEvent" "Reference"

instance ApplyEventToReference EditKnowledgeModelEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditKnowledgeModelEvent" "Reference"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToReference AddChapterEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddChapterEvent" "Reference"

instance ApplyEventToReference EditChapterEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditChapterEvent" "Reference"

instance ApplyEventToReference DeleteChapterEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteChapterEvent" "Reference"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToReference AddQuestionEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddQuestionEvent" "Reference"

instance ApplyEventToReference EditQuestionEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditQuestionEvent" "Reference"

instance ApplyEventToReference DeleteQuestionEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteQuestionEvent" "Reference"

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToReference AddAnswerEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddAnswerEvent" "Reference"

instance ApplyEventToReference EditAnswerEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditAnswerEvent" "Reference"

instance ApplyEventToReference DeleteAnswerEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteAnswerEvent" "Reference"

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToReference AddFollowUpQuestionEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddFollowUpQuestionEvent" "Reference"

instance ApplyEventToReference EditFollowUpQuestionEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditFollowUpQuestionEvent" "Reference"

instance ApplyEventToReference DeleteFollowUpQuestionEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteFollowUpQuestionEvent" "Reference"

-- -----------------------------------
-- ANSWER-ITEM-TEMPLATE-QUESTIONS ----
-- -----------------------------------
instance ApplyEventToReference AddAnswerItemTemplateQuestionEvent where
  applyEventToReference _ _ =
    Left . MigratorError $
    _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddAnswerItemTemplateQuestionEvent" "Reference"

instance ApplyEventToReference EditAnswerItemTemplateQuestionEvent where
  applyEventToReference _ _ =
    Left . MigratorError $
    _ERROR_MT_APPLICATOR__BAD_APPLICATION "EditAnswerItemTemplateQuestionEvent" "Reference"

instance ApplyEventToReference DeleteAnswerItemTemplateQuestionEvent where
  applyEventToReference _ _ =
    Left . MigratorError $
    _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteAnswerItemTemplateQuestionEvent" "Reference"

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToReference AddExpertEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "AddExpertEvent" "Reference"

instance ApplyEventToReference EditExpertEvent where
  applyEventToReference e ref = ref

instance ApplyEventToReference DeleteExpertEvent where
  applyEventToReference _ _ =
    Left . MigratorError $ _ERROR_MT_APPLICATOR__BAD_APPLICATION "DeleteExpertEvent" "Reference"

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToReference AddReferenceEvent where
  applyEventToReference _ _ = undefined

instance ApplyEventToReference EditReferenceEvent where
  applyEventToReference e (Left error) = Left error
  applyEventToReference e (Right ref) =
    if equalsUuid e ref
      then Right $ applyChapter ref
      else Right ref
    where
      applyChapter ref = applyValue (e ^. chapter) ref chapter

instance ApplyEventToReference DeleteReferenceEvent where
  applyEventToReference _ _ = undefined
