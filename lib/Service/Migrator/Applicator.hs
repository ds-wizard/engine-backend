module Service.Migrator.Applicator where

import Control.Lens

import Common.Error
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

runApplicator :: Maybe KnowledgeModel -> [Event] -> Either AppError KnowledgeModel
runApplicator mKM events =
  case foldl foldEvent (Right mKM) events of
    Left error -> Left error
    Right Nothing -> Left . MigratorError $ "Unspecified problem in building Knowledge Model happened"
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

applyValue (Just val) ch setter = ch & setter .~ val
applyValue Nothing ch setter = ch

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class ApplyEventToKM e where
  applyEventToKM :: e -> Either AppError (Maybe KnowledgeModel) -> Either AppError (Maybe KnowledgeModel)

passToChapters _ (Left error) = Left error
passToChapters e (Right Nothing) = Left . MigratorError $ "You have to create Knowledge Model at first"
passToChapters e (Right (Just km)) =
  case eModifiedChapters of
    Left error -> Left error
    Right modifiedChapters -> Right . Just $ km & kmChapters .~ modifiedChapters
  where
    eModifiedChapters = foldl foldOneChapter (Right []) (km ^. kmChapters)
    foldOneChapter :: Either AppError [Chapter] -> Chapter -> Either AppError [Chapter]
    foldOneChapter (Left error) _ = Left error
    foldOneChapter (Right chapters) chapter =
      case applyEventToChapter e (Right chapter) of
        Left error -> Left error
        Right appliedChapter -> Right $ chapters ++ [appliedChapter]

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToKM AddKnowledgeModelEvent where
  applyEventToKM _ (Left error) = Left error
  applyEventToKM e (Right (Just _)) = Left . MigratorError $ "Knowledge Model is already created"
  applyEventToKM e (Right Nothing) =
    Right . Just $ KnowledgeModel {_kmUuid = e ^. akmKmUuid, _kmName = e ^. akmName, _kmChapters = []}

instance ApplyEventToKM EditKnowledgeModelEvent where
  applyEventToKM _ (Left error) = Left error
  applyEventToKM e (Right Nothing) = Left . MigratorError $ "You have to create Knowledge Model at first"
  applyEventToKM e (Right (Just km)) = Right . Just . applyChapterIds . applyName $ km
    where
      applyName km = applyValue (e ^. ekmName) km kmName
      applyChapterIds km = applyValue (e ^. ekmChapterIds) km kmChangeChapterIdsOrder

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToKM AddChapterEvent where
  applyEventToKM _ (Left error) = Left error
  applyEventToKM e (Right Nothing) = Left . MigratorError $ "You have to create Knowledge Model at first"
  applyEventToKM e (Right (Just km)) = Right . Just $ km & kmChapters .~ modifiedChapters
    where
      modifiedChapters = km ^. kmChapters ++ [newChapter]
      newChapter =
        Chapter {_chUuid = e ^. achChapterUuid, _chTitle = e ^. achTitle, _chText = e ^. achText, _chQuestions = []}

instance ApplyEventToKM EditChapterEvent where
  applyEventToKM = passToChapters

instance ApplyEventToKM DeleteChapterEvent where
  applyEventToKM _ (Left error) = Left error
  applyEventToKM e (Right Nothing) = Left . MigratorError $ "You have to create Knowledge Model at first"
  applyEventToKM e (Right (Just km)) =
    if equalsUuid e km
      then Right . Just $ km & kmChapters .~ modifiedChapters
      else Right . Just $ km
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
  applyEventToChapter :: e -> Either AppError Chapter -> Either AppError Chapter

passToQuestions _ (Left error) = Left error
passToQuestions e (Right ch) =
  case eModifiedQuestions of
    Left error -> Left error
    Right modifiedQuestions -> Right $ ch & chQuestions .~ modifiedQuestions
  where
    eModifiedQuestions = foldl foldOneQuestion (Right []) (ch ^. chQuestions)
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
  applyEventToChapter _ _ = Left . MigratorError $ "You can't apply AddKnowledgeModelEvent to Chapter"

instance ApplyEventToChapter EditKnowledgeModelEvent where
  applyEventToChapter _ _ = Left . MigratorError $ "You can't apply EditKnowledgeModelEvent to Chapter"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToChapter AddChapterEvent where
  applyEventToChapter _ _ = Left . MigratorError $ "You can't apply AddChapterEvent to Chapter"

instance ApplyEventToChapter EditChapterEvent where
  applyEventToChapter _ (Left error) = Left error
  applyEventToChapter e (Right ch) =
    if equalsUuid e ch
      then Right . applyQuestionIds . applyText . applyTitle $ ch
      else Right ch
    where
      applyTitle ch = applyValue (e ^. echTitle) ch chTitle
      applyText ch = applyValue (e ^. echText) ch chText
      applyQuestionIds ch = applyValue (e ^. echQuestionIds) ch chChangeQuestionIdsOrder

instance ApplyEventToChapter DeleteChapterEvent where
  applyEventToChapter _ _ = Left . MigratorError $ "You can't apply DeleteChapterEvent to Chapter"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToChapter AddQuestionEvent where
  applyEventToChapter _ (Left error) = Left error
  applyEventToChapter e (Right ch) =
    if equalsUuid e ch
      then Right $ ch & chQuestions .~ modifiedQuestions
      else Right ch
    where
      modifiedQuestions = ch ^. chQuestions ++ [newQuestion]
      newQuestion =
        Question
        { _qUuid = e ^. aqQuestionUuid
        , _qShortUuid = e ^. aqShortQuestionUuid
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
  applyEventToChapter _ (Left error) = Left error
  applyEventToChapter e (Right ch) =
    if equalsUuid e ch
      then Right $ ch & chQuestions .~ modifiedQuestions
      else passToQuestions e (Right ch)
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
  applyEventToQuestion :: e -> Either AppError Question -> Either AppError Question

passToAnswers _ (Left error) = Left error
passToAnswers e (Right q) =
  case eModifiedAnswers of
    Left error -> Left error
    Right modifiedAnswers -> Right $ q & qAnswers .~ modifiedAnswers
  where
    eModifiedAnswers = foldl foldOneAnswer (Right []) (q ^. qAnswers)
    foldOneAnswer :: Either AppError [Answer] -> Answer -> Either AppError [Answer]
    foldOneAnswer (Left error) _ = Left error
    foldOneAnswer (Right answers) answer =
      case applyEventToAnswer e (Right answer) of
        Left error -> Left error
        Right appliedAnswers -> Right $ answers ++ [appliedAnswers]

passToExperts _ (Left error) = Left error
passToExperts e (Right q) =
  case eModifiedExperts of
    Left error -> Left error
    Right modifiedExperts -> Right $ q & qExperts .~ modifiedExperts
  where
    eModifiedExperts = foldl foldOneExpert (Right []) (q ^. qExperts)
    foldOneExpert :: Either AppError [Expert] -> Expert -> Either AppError [Expert]
    foldOneExpert (Left error) _ = Left error
    foldOneExpert (Right experts) expert =
      case applyEventToExpert e (Right expert) of
        Left error -> Left error
        Right appliedExpert -> Right $ experts ++ [appliedExpert]

passToReferences _ (Left error) = Left error
passToReferences e (Right q) =
  case eModifiedReferences of
    Left error -> Left error
    Right modifiedReferences -> Right $ q & qReferences .~ modifiedReferences
  where
    eModifiedReferences = foldl foldOneReference (Right []) (q ^. qReferences)
    foldOneReference :: Either AppError [Reference] -> Reference -> Either AppError [Reference]
    foldOneReference (Left error) _ = Left error
    foldOneReference (Right references) reference =
      case applyEventToReference e (Right reference) of
        Left error -> Left error
        Right appliedReference -> Right $ references ++ [appliedReference]

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToQuestion AddKnowledgeModelEvent where
  applyEventToQuestion _ _ = Left . MigratorError $ "You can't apply AddKnowledgeModelEvent to Question"

instance ApplyEventToQuestion EditKnowledgeModelEvent where
  applyEventToQuestion _ _ = Left . MigratorError $ "You can't apply EditKnowledgeModelEvent to Question"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToQuestion AddChapterEvent where
  applyEventToQuestion _ _ = Left . MigratorError $ "You can't apply AddChapterEvent to Question"

instance ApplyEventToQuestion EditChapterEvent where
  applyEventToQuestion _ _ = Left . MigratorError $ "You can't apply EditChapterEvent to Question"

instance ApplyEventToQuestion DeleteChapterEvent where
  applyEventToQuestion _ _ = Left . MigratorError $ "You can't apply DeleteChapterEvent to Question"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToQuestion AddQuestionEvent where
  applyEventToQuestion = passToAnswers

instance ApplyEventToQuestion EditQuestionEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right .
           applyReferenceIds . applyExpertIds . applyAnwerIds . applyText . applyTitle . applyType . applyShortUuid $
           q
      else passToAnswers e (Right q)
    where
      applyShortUuid q = applyValue (e ^. eqShortQuestionUuid) q qShortUuid
      applyType q = applyValue (e ^. eqType) q qType
      applyTitle q = applyValue (e ^. eqTitle) q qTitle
      applyText q = applyValue (e ^. eqText) q qText
      applyAnwerIds q = applyValue (e ^. eqAnswerIds) q qChangeAnwerIdsOrder
      applyExpertIds q = applyValue (e ^. eqExpertIds) q qChangeExpertIdsOrder
      applyReferenceIds q = applyValue (e ^. eqReferenceIds) q qChangeReferenceIdsOrder

instance ApplyEventToQuestion DeleteQuestionEvent where
  applyEventToQuestion = passToAnswers

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToQuestion AddAnswerEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & qAnswers .~ modifiedAnswers
      else passToAnswers e (Right q)
    where
      modifiedAnswers = q ^. qAnswers ++ [newAnswer]
      newAnswer =
        Answer
        {_ansUuid = e ^. aansAnswerUuid, _ansLabel = e ^. aansLabel, _ansAdvice = e ^. aansAdvice, _ansFollowUps = []}

instance ApplyEventToQuestion EditAnswerEvent where
  applyEventToQuestion = passToAnswers

instance ApplyEventToQuestion DeleteAnswerEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & qAnswers .~ modifiedAnswers
      else passToAnswers e (Right q)
    where
      modifiedAnswers = filter (not . equalsUuid e) (q ^. qAnswers)

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToQuestion AddFollowUpQuestionEvent where
  applyEventToQuestion = passToAnswers

instance ApplyEventToQuestion EditFollowUpQuestionEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right . applyReferenceIds . applyExpertIds . applyAnwerIds . applyText . applyTitle . applyType $ q
      else passToAnswers e (Right q)
    where
      applyShortQuestionId q = applyValue (e ^. efuqShortQuestionUuid) q qShortUuid
      applyType q = applyValue (e ^. efuqType) q qType
      applyTitle q = applyValue (e ^. efuqTitle) q qTitle
      applyText q = applyValue (e ^. efuqText) q qText
      applyAnwerIds q = applyValue (e ^. efuqAnswerIds) q qChangeAnwerIdsOrder
      applyExpertIds q = applyValue (e ^. efuqExpertIds) q qChangeExpertIdsOrder
      applyReferenceIds q = applyValue (e ^. efuqReferenceIds) q qChangeReferenceIdsOrder

instance ApplyEventToQuestion DeleteFollowUpQuestionEvent where
  applyEventToQuestion = passToAnswers

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToQuestion AddExpertEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & qExperts .~ modifiedExperts
      else passToAnswers e (Right q)
    where
      modifiedExperts = q ^. qExperts ++ [newExpert]
      newExpert = Expert {_expUuid = e ^. aexpExpertUuid, _expName = e ^. aexpName, _expEmail = e ^. aexpEmail}

instance ApplyEventToQuestion EditExpertEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) = passToReferences e . passToExperts e . passToAnswers e . Right $ q

instance ApplyEventToQuestion DeleteExpertEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & qExperts .~ modifiedExperts
      else passToAnswers e (Right q)
    where
      modifiedExperts = filter (not . equalsUuid e) (q ^. qExperts)

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToQuestion AddReferenceEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & qReferences .~ modifiedReferences
      else passToAnswers e (Right q)
    where
      modifiedReferences = q ^. qReferences ++ [newReference]
      newReference = Reference {_refUuid = e ^. arefReferenceUuid, _refChapter = e ^. arefChapter}

instance ApplyEventToQuestion EditReferenceEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) = passToReferences e . passToExperts e . passToAnswers e . Right $ q

instance ApplyEventToQuestion DeleteReferenceEvent where
  applyEventToQuestion e (Left error) = Left error
  applyEventToQuestion e (Right q) =
    if equalsUuid e q
      then Right $ q & qReferences .~ modifiedReferences
      else passToAnswers e (Right q)
    where
      modifiedReferences = filter (not . equalsUuid e) (q ^. qReferences)

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
    Right modifiedFollowUps -> Right $ ans & ansFollowUps .~ modifiedFollowUps
  where
    eModifiedFollowUps = foldl foldOneFollowUps (Right []) (ans ^. ansFollowUps)
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
  applyEventToAnswer _ _ = Left . MigratorError $ "You can't apply AddKnowledgeModelEvent to Answer"

instance ApplyEventToAnswer EditKnowledgeModelEvent where
  applyEventToAnswer _ _ = Left . MigratorError $ "You can't apply EditKnowledgeModelEvent to Answer"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToAnswer AddChapterEvent where
  applyEventToAnswer _ _ = Left . MigratorError $ "You can't apply AddChapterEvent to Answer"

instance ApplyEventToAnswer EditChapterEvent where
  applyEventToAnswer _ _ = Left . MigratorError $ "You can't apply EditChapterEvent to Answer"

instance ApplyEventToAnswer DeleteChapterEvent where
  applyEventToAnswer _ _ = Left . MigratorError $ "You can't apply DeleteChapterEvent to Answer"

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
      applyLabel ans = applyValue (e ^. eansLabel) ans ansLabel
      applyAdvice ans = applyValue (e ^. eansAdvice) ans ansAdvice
      applyFollowUps ans = applyValue (e ^. eansFollowUpIds) ans ansChangeFollowUpIdsOrder

instance ApplyEventToAnswer DeleteAnswerEvent where
  applyEventToAnswer = passToFollowUps

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToAnswer AddFollowUpQuestionEvent where
  applyEventToAnswer e (Left error) = Left error
  applyEventToAnswer e (Right ans) =
    if equalsUuid e ans
      then Right $ ans & ansFollowUps .~ modifiedFollowUps
      else passToFollowUps e (Right ans)
    where
      modifiedFollowUps = ans ^. ansFollowUps ++ [newFollowUp]
      newFollowUp =
        Question
        { _qUuid = e ^. afuqQuestionUuid
        , _qShortUuid = e ^. afuqShortQuestionUuid
        , _qType = e ^. afuqType
        , _qTitle = e ^. afuqTitle
        , _qText = e ^. afuqText
        , _qAnswers = []
        , _qReferences = []
        , _qExperts = []
        }

instance ApplyEventToAnswer EditFollowUpQuestionEvent where
  applyEventToAnswer = passToFollowUps

instance ApplyEventToAnswer DeleteFollowUpQuestionEvent where
  applyEventToAnswer e (Left error) = Left error
  applyEventToAnswer e (Right ans) =
    if equalsUuid e ans
      then Right $ ans & ansFollowUps .~ modifiedFollowUps
      else passToFollowUps e (Right ans)
    where
      modifiedFollowUps = filter (not . equalsUuid e) (ans ^. ansFollowUps)

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
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply AddKnowledgeModelEvent to Expert"

instance ApplyEventToExpert EditKnowledgeModelEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply EditKnowledgeModelEvent to Expert"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToExpert AddChapterEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply AddChapterEvent to Expert"

instance ApplyEventToExpert EditChapterEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply EditChapterEvent to Expert"

instance ApplyEventToExpert DeleteChapterEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply DeleteChapterEvent to Expert"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToExpert AddQuestionEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply AddQuestionEvent to Expert"

instance ApplyEventToExpert EditQuestionEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply EditQuestionEvent to Expert"

instance ApplyEventToExpert DeleteQuestionEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply DeleteQuestionEvent to Expert"

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToExpert AddAnswerEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply AddAnswerEvent to Expert"

instance ApplyEventToExpert EditAnswerEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply EditAnswerEvent to Expert"

instance ApplyEventToExpert DeleteAnswerEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply DeleteAnswerEvent to Expert"

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToExpert AddFollowUpQuestionEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply AddFollowUpQuestionEvent to Expert"

instance ApplyEventToExpert EditFollowUpQuestionEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply EditFollowUpQuestionEvent to Expert"

instance ApplyEventToExpert DeleteFollowUpQuestionEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply DeleteFollowUpQuestionEvent to Expert"

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToExpert AddExpertEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply AddExpertEvent to Expert"

instance ApplyEventToExpert EditExpertEvent where
  applyEventToExpert e (Left error) = Left error
  applyEventToExpert e (Right exp) =
    if equalsUuid e exp
      then Right $ applyEmail . applyName $ exp
      else Right exp
    where
      applyName exp = applyValue (e ^. eexpName) exp expName
      applyEmail exp = applyValue (e ^. eexpEmail) exp expEmail

instance ApplyEventToExpert DeleteExpertEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply DeleteExpertEvent to Expert"

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToExpert AddReferenceEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply AddReferenceEvent to Expert"

instance ApplyEventToExpert EditReferenceEvent where
  applyEventToExpert e exp = exp

instance ApplyEventToExpert DeleteReferenceEvent where
  applyEventToExpert _ _ = Left . MigratorError $ "You can't apply DeleteReferenceEvent to Expert"

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
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply AddKnowledgeModelEvent to Reference"

instance ApplyEventToReference EditKnowledgeModelEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply EditKnowledgeModelEvent to Reference"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToReference AddChapterEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply AddChapterEvent to Reference"

instance ApplyEventToReference EditChapterEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply EditChapterEvent to Reference"

instance ApplyEventToReference DeleteChapterEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply DeleteChapterEvent to Reference"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToReference AddQuestionEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply AddQuestionEvent to Reference"

instance ApplyEventToReference EditQuestionEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply EditQuestionEvent to Reference"

instance ApplyEventToReference DeleteQuestionEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply DeleteQuestionEvent to Reference"

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToReference AddAnswerEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply AddAnswerEvent to Reference"

instance ApplyEventToReference EditAnswerEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply EditAnswerEvent to Reference"

instance ApplyEventToReference DeleteAnswerEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply DeleteAnswerEvent to Reference"

-- ------------------------
-- FOLLOW-UP QUESTIONS ----
-- ------------------------
instance ApplyEventToReference AddFollowUpQuestionEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply AddFollowUpQuestionEvent to Reference"

instance ApplyEventToReference EditFollowUpQuestionEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply EditFollowUpQuestionEvent to Reference"

instance ApplyEventToReference DeleteFollowUpQuestionEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply DeleteFollowUpQuestionEvent to Reference"

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToReference AddExpertEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply AddExpertEvent to Reference"

instance ApplyEventToReference EditExpertEvent where
  applyEventToReference e ref = ref

instance ApplyEventToReference DeleteExpertEvent where
  applyEventToReference _ _ = Left . MigratorError $ "You can't apply DeleteExpertEvent to Reference"

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
      applyChapter ref = applyValue (e ^. erefChapter) ref refChapter

instance ApplyEventToReference DeleteReferenceEvent where
  applyEventToReference _ _ = undefined
