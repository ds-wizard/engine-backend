module Service.Migration.KnowledgeModel.Applicator.ApplyEventInstances where

import Control.Lens

import LensesConfig
import Localization
import Model.Error.Error
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventAccessors
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Service.Migration.KnowledgeModel.Applicator.ApplyEvent
import Service.Migration.KnowledgeModel.Applicator.Errors
import Service.Migration.KnowledgeModel.Applicator.Modifiers
import Service.Migration.KnowledgeModel.Applicator.Utils

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO KNOWLEDGE MODEL
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyEventToChapters e path km mChUuid =
  hFoldl foldOneChapter (Right []) (km ^. chapters) $ \mChapters -> Right . Just $ km & chapters .~ mChapters
  where
    foldOneChapter :: Either AppError [Chapter] -> Chapter -> Either AppError [Chapter]
    foldOneChapter (Left error) _ = Left error
    foldOneChapter (Right chs) chapter =
      case mChUuid of
        Just chUuid ->
          if chapter ^. uuid == chUuid
            then applyToOneChapter chs chapter
            else Right $ chs ++ [chapter]
        Nothing -> applyToOneChapter chs chapter
    applyToOneChapter :: [Chapter] -> Chapter -> Either AppError [Chapter]
    applyToOneChapter chs chapter =
      heApplyEventToChapter e path (Right chapter) $ \appliedChapter -> Right $ chs ++ [appliedChapter]

passToChapters _ _ (Left error) = Left error
passToChapters e _ (Right Nothing) = errorEditNonExistingThing e
passToChapters e (_:pChUuid:restOfPath) (Right (Just km)) =
  applyEventToChapters e (pChUuid : restOfPath) km (Just $ pChUuid ^. uuid)
passToChapters e _ _ = errorEmptyPath e

broadcastToChapters _ _ (Left error) = Left error
broadcastToChapters e _ (Right Nothing) = errorEditNonExistingThing e
broadcastToChapters e path (Right (Just km)) = applyEventToChapters e path km Nothing

-- --------------------------------------------
applyEventToTags e path km tUuid =
  hFoldl foldOneTag (Right []) (km ^. tags) $ \mTags -> Right . Just $ km & tags .~ mTags
  where
    foldOneTag :: Either AppError [Tag] -> Tag -> Either AppError [Tag]
    foldOneTag (Left error) _ = Left error
    foldOneTag (Right ts) tag =
      if tag ^. uuid == tUuid
        then heApplyEventToTag e path (Right tag) $ \appliedTag -> Right $ ts ++ [appliedTag]
        else Right $ ts ++ [tag]

-- --------------------------------------------
applyEventToIntegrations e path km iUuid =
  hFoldl foldOneIntegration (Right []) (km ^. integrations) $ \mIntegrations ->
    Right . Just $ km & integrations .~ mIntegrations
  where
    foldOneIntegration :: Either AppError [Integration] -> Integration -> Either AppError [Integration]
    foldOneIntegration (Left error) _ = Left error
    foldOneIntegration (Right is) integration =
      if integration ^. uuid == iUuid
        then heApplyEventToIntegration e path (Right integration) $ \appliedIntegration ->
               Right $ is ++ [appliedIntegration]
        else Right $ is ++ [integration]

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToKM AddKnowledgeModelEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right (Just _)) = Left . MigratorError $ _ERROR_KMMT_VALIDATION_APPLICATOR__KM_UNIQUENESS
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
  applyEventToKM e (_:restOfPath) (Right (Just km)) = applyEventToChapters e restOfPath km (Just $ e ^. chapterUuid)
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

-- -------------------
-- TAGS --------------
-- -------------------
instance ApplyEventToKM AddTagEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e _ (Right (Just km)) = Right . Just $ addTag km (createTag e)

instance ApplyEventToKM EditTagEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e (_:restOfPath) (Right (Just km)) = applyEventToTags e restOfPath km (e ^. tagUuid)
  applyEventToKM e [] _ = errorEmptyPath e

instance ApplyEventToKM DeleteTagEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e path (Right (Just km)) = removeAllTagReferencesFromKM removeTagFromKM
    where
      removeTagFromKM = Right . Just $ deleteTag km (e ^. tagUuid)
      removeAllTagReferencesFromKM = broadcastToChapters e path

-- -------------------
-- INTEGRATIONS ------
-- -------------------
instance ApplyEventToKM AddIntegrationEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e _ (Right (Just km)) = Right . Just $ addIntegration km (createIntegration e)

instance ApplyEventToKM EditIntegrationEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e (_:restOfPath) (Right (Just km)) = applyToAllItsChildren applyToIntegrations
    where
      applyToAllItsChildren = broadcastToChapters e restOfPath
      applyToIntegrations = applyEventToIntegrations e restOfPath km (e ^. integrationUuid)
  applyEventToKM e [] _ = errorEmptyPath e

instance ApplyEventToKM DeleteIntegrationEvent where
  applyEventToKM _ _ (Left error) = Left error
  applyEventToKM e _ (Right Nothing) = errorEditNonExistingThing e
  applyEventToKM e path (Right (Just km)) = changeAllIntegrationQuestionsToValueQuetions removeIntegrationFromKM
    where
      removeIntegrationFromKM = Right . Just $ deleteIntegration km (e ^. integrationUuid)
      changeAllIntegrationQuestionsToValueQuetions = broadcastToChapters e path

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO CHAPTER
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyEventToQuestions e path ch mQUuid =
  hFoldl foldOneQuestion (Right []) (ch ^. questions) $ \mQuestions -> Right $ ch & questions .~ mQuestions
  where
    foldOneQuestion :: Either AppError [Question] -> Question -> Either AppError [Question]
    foldOneQuestion (Left error) _ = Left error
    foldOneQuestion (Right qs) question =
      case mQUuid of
        Just qUuid ->
          if getQuestionUuid question == qUuid
            then applyToOneQuestion qs question
            else Right $ qs ++ [question]
        Nothing -> applyToOneQuestion qs question
    applyToOneQuestion :: [Question] -> Question -> Either AppError [Question]
    applyToOneQuestion qs question =
      heApplyEventToQuestion e path (Right question) $ \appliedQuestion -> Right $ qs ++ [appliedQuestion]

passToQuestions _ _ (Left error) = Left error
passToQuestions e (_:pQUuid:restOfPath) (Right ch) =
  applyEventToQuestions e (pQUuid : restOfPath) ch (Just $ pQUuid ^. uuid)
passToQuestions e _ _ = errorEmptyPath e

broadcastToQuestions _ _ (Left error) = Left error
broadcastToQuestions e path (Right ch) = applyEventToQuestions e path ch Nothing

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
  applyEventToChapter e (pChUuid:[]) (Right ch) = Right $ addQuestion ch (createQuestion e)
  applyEventToChapter e path (Right ch) = passToQuestions e path (Right ch)

instance ApplyEventToChapter EditQuestionEvent where
  applyEventToChapter _ _ (Left error) = Left error
  applyEventToChapter e (_:[]) (Right ch) = applyEventToQuestions e [] ch (Just $ getEventQuestionUuid e)
  applyEventToChapter e path (Right ch) = passToQuestions e path (Right ch)

instance ApplyEventToChapter DeleteQuestionEvent where
  applyEventToChapter _ _ (Left error) = Left error
  applyEventToChapter e (pChUuid:[]) (Right ch) = Right $ deleteQuestion ch (getEventQuestionUuid e)
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

-- -------------------
-- TAGS --------------
-- -------------------
instance ApplyEventToChapter AddTagEvent where
  applyEventToChapter e _ _ = errorIllegalState e "AddTagEvent" "Chapter"

instance ApplyEventToChapter EditTagEvent where
  applyEventToChapter e _ _ = errorIllegalState e "EditTagEvent" "Chapter"

instance ApplyEventToChapter DeleteTagEvent where
  applyEventToChapter = broadcastToQuestions

-- -------------------
-- INTEGRATIONS ------
-- -------------------
instance ApplyEventToChapter AddIntegrationEvent where
  applyEventToChapter e _ _ = errorIllegalState e "AddIntegrationEvent" "Chapter"

instance ApplyEventToChapter EditIntegrationEvent where
  applyEventToChapter = broadcastToQuestions

instance ApplyEventToChapter DeleteIntegrationEvent where
  applyEventToChapter = broadcastToQuestions

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO QUESTION
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
passToAnswersAndItemTemplateQuestions e path eQ = passToItemTemplateQuestions e path $ passToAnswers e path eQ

broadcastToAnswersAndItemTemplateQuestions e path eQ =
  broadcastToItemTemplateQuestions e path $ broadcastToAnswers e path eQ

-- --------------------------------------------
applyEventToAnswers e path q mAnsUuid =
  hFoldl foldOneAnswer (Right []) (q ^. answers) $ \as -> Right . OptionsQuestion' $ q & answers .~ as
  where
    foldOneAnswer :: Either AppError [Answer] -> Answer -> Either AppError [Answer]
    foldOneAnswer (Left error) _ = Left error
    foldOneAnswer (Right as) answer =
      case mAnsUuid of
        Just ansUuid ->
          if answer ^. uuid == ansUuid
            then applyToOneAnswer as answer
            else Right $ as ++ [answer]
        Nothing -> applyToOneAnswer as answer
    applyToOneAnswer :: [Answer] -> Answer -> Either AppError [Answer]
    applyToOneAnswer as answer =
      heApplyEventToAnswer e path (Right answer) $ \appliedAnswer -> Right $ as ++ [appliedAnswer]

passToAnswers _ _ (Left error) = Left error
passToAnswers e (_:pAnsUuid:restOfPath) (Right (OptionsQuestion' q)) =
  applyEventToAnswers e (pAnsUuid : restOfPath) q (Just $ pAnsUuid ^. uuid)
passToAnswers e (_:pAnsUuid:restOfPath) (Right q) = Right q
passToAnswers e _ _ = errorEmptyPath e

broadcastToAnswers _ _ (Left error) = Left error
broadcastToAnswers e path (Right (OptionsQuestion' q)) = applyEventToAnswers e path q Nothing
broadcastToAnswers e path eQ = eQ

-- --------------------------------------------
applyEventToItemTemplateQuestions e path q mQUuid =
  hFoldl foldOneQuestion (Right []) (q ^. itemTemplateQuestions) $ \mQuestions ->
    Right . ListQuestion' $ q & itemTemplateQuestions .~ mQuestions
  where
    foldOneQuestion :: Either AppError [Question] -> Question -> Either AppError [Question]
    foldOneQuestion (Left error) _ = Left error
    foldOneQuestion (Right qs) question =
      case mQUuid of
        Just qUuid ->
          if getQuestionUuid question == qUuid
            then applyToOneQuestion qs question
            else Right $ qs ++ [question]
        Nothing -> applyToOneQuestion qs question
    applyToOneQuestion :: [Question] -> Question -> Either AppError [Question]
    applyToOneQuestion qs question =
      heApplyEventToQuestion e path (Right question) $ \appliedQuestion -> Right $ qs ++ [appliedQuestion]

passToItemTemplateQuestions _ _ (Left error) = Left error
passToItemTemplateQuestions e (_:pQUuid:restOfPath) (Right (ListQuestion' q)) =
  applyEventToItemTemplateQuestions e (pQUuid : restOfPath) q (Just $ pQUuid ^. uuid)
passToItemTemplateQuestions e (_:pQUuid:restOfPath) (Right q) = Right q
passToItemTemplateQuestions e _ _ = errorEmptyPath e

broadcastToItemTemplateQuestions _ _ (Left error) = Left error
broadcastToItemTemplateQuestions e path (Right (ListQuestion' q)) = applyEventToItemTemplateQuestions e path q Nothing
broadcastToItemTemplateQuestions e path (Right q) = Right q

-- --------------------------------------------
applyEventToExperts e path q expUuid =
  hFoldl foldOneExpert (Right []) (getExperts q) $ \es -> Right $ q & qChangeExperts .~ es
  where
    foldOneExpert :: Either AppError [Expert] -> Expert -> Either AppError [Expert]
    foldOneExpert (Left error) _ = Left error
    foldOneExpert (Right es) expert =
      if expert ^. uuid == expUuid
        then heApplyEventToExpert e path (Right expert) $ \appliedExpert -> Right $ es ++ [appliedExpert]
        else Right $ es ++ [expert]

-- --------------------------------------------
applyEventToReferences e path q refUuid =
  hFoldl foldOneReference (Right []) (getReferences q) $ \rs -> Right $ q & qChangeReferences .~ rs
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
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ addItemTemplateQuestion q (createQuestion e)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

instance ApplyEventToQuestion EditQuestionEvent where
  applyEventToQuestion _ _ (Left error) = Left error
  applyEventToQuestion e [] (Right q) = Right $ editQuestion e q
  applyEventToQuestion e (pQUuid:[]) (Right (ListQuestion' q)) =
    applyEventToItemTemplateQuestions e [] q (Just $ getEventQuestionUuid e)
  applyEventToQuestion e path (Right q) = passToAnswersAndItemTemplateQuestions e path (Right q)

instance ApplyEventToQuestion DeleteQuestionEvent where
  applyEventToQuestion _ _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ deleteItemTemplateQuestion q (getEventQuestionUuid e)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToQuestion AddAnswerEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ addAnswer q (createAnswer e)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

instance ApplyEventToQuestion EditAnswerEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right (OptionsQuestion' q)) = applyEventToAnswers e [] q (Just $ e ^. answerUuid)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

instance ApplyEventToQuestion DeleteAnswerEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ deleteAnswer q (e ^. answerUuid)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToQuestion AddExpertEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ addExpert q (createExpert e)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

instance ApplyEventToQuestion EditExpertEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = applyEventToExperts e [] q (e ^. expertUuid)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

instance ApplyEventToQuestion DeleteExpertEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ deleteExpert q (e ^. expertUuid)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToQuestion AddReferenceEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ addReference q (createReference e)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

instance ApplyEventToQuestion EditReferenceEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = applyEventToReferences e [] q (getEventNodeUuid e)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

instance ApplyEventToQuestion DeleteReferenceEvent where
  applyEventToQuestion e _ (Left error) = Left error
  applyEventToQuestion e (pQUuid:[]) (Right q) = Right $ deleteReference q (getEventNodeUuid e)
  applyEventToQuestion e path eQ = passToAnswersAndItemTemplateQuestions e path eQ

-- -------------------
-- TAGS --------------
-- -------------------
instance ApplyEventToQuestion AddTagEvent where
  applyEventToQuestion e _ _ = errorIllegalState e "AddTagEvent" "Question"

instance ApplyEventToQuestion EditTagEvent where
  applyEventToQuestion e _ _ = errorIllegalState e "EditTagEvent" "Question"

instance ApplyEventToQuestion DeleteTagEvent where
  applyEventToQuestion _ _ (Left error) = Left error
  applyEventToQuestion e path (Right q) = removeFromAllItsChildren removeTagsFromQuestion
    where
      removeTagsFromQuestion = Right $ q & qChangeTagUuids .~ (filter (\tUuid -> tUuid /= e ^. tagUuid) (getTagUuids q))
      removeFromAllItsChildren = broadcastToAnswersAndItemTemplateQuestions e path

-- -------------------
-- INTEGRATIONS ------
-- -------------------
instance ApplyEventToQuestion AddIntegrationEvent where
  applyEventToQuestion e _ _ = errorIllegalState e "AddIntegrationEvent" "Question"

instance ApplyEventToQuestion EditIntegrationEvent where
  applyEventToQuestion _ _ (Left error) = Left error
  applyEventToQuestion e path (Right q) = applyToAllItsChildren updateProps
    where
      updateProps = Right . updateIntegrationProps e $ q
      applyToAllItsChildren = broadcastToAnswersAndItemTemplateQuestions e path

instance ApplyEventToQuestion DeleteIntegrationEvent where
  applyEventToQuestion _ _ (Left error) = Left error
  applyEventToQuestion e path (Right q) = applyToAllItsChildren . changeIntegrationQuestionToValueQuetion $ q
    where
      changeIntegrationQuestionToValueQuetion (IntegrationQuestion' iq) =
        Right . convertToValueQuestion . IntegrationQuestion' $ iq
      changeIntegrationQuestionToValueQuetion q' = Right q'
      applyToAllItsChildren = broadcastToAnswersAndItemTemplateQuestions e path

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO ANSWER
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
applyEventToFollowUps e path answer mQUuid =
  hFoldl foldOneFollowUps (Right []) (answer ^. followUps) $ \mFollowUps -> Right $ answer & followUps .~ mFollowUps
  where
    foldOneFollowUps :: Either AppError [Question] -> Question -> Either AppError [Question]
    foldOneFollowUps (Left error) _ = Left error
    foldOneFollowUps (Right qs) question =
      case mQUuid of
        Just qUuid ->
          if getQuestionUuid question == qUuid
            then applyEventToFollowUp qs question
            else Right $ qs ++ [question]
        Nothing -> applyEventToFollowUp qs question
    applyEventToFollowUp :: [Question] -> Question -> Either AppError [Question]
    applyEventToFollowUp qs question =
      heApplyEventToQuestion e path (Right question) $ \appliedQuestion -> Right $ qs ++ [appliedQuestion]

passToFollowUps _ _ (Left error) = Left error
passToFollowUps e (_:pQUuid:restOfPath) (Right answer) =
  applyEventToFollowUps e (pQUuid : restOfPath) answer (Just $ pQUuid ^. uuid)
passToFollowUps e _ _ = errorEmptyPath e

broadcastToFollowUps _ _ (Left error) = Left error
broadcastToFollowUps e path (Right answer) = applyEventToFollowUps e path answer Nothing

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
  applyEventToAnswer e (ansUuid:[]) (Right ans) = Right $ addFuQuestion ans (createQuestion e)
  applyEventToAnswer e path eAns = passToFollowUps e path eAns

instance ApplyEventToAnswer EditQuestionEvent where
  applyEventToAnswer _ _ (Left error) = Left error
  applyEventToAnswer e (ansUuid:[]) (Right ans) = applyEventToFollowUps e [] ans (Just $ getEventQuestionUuid e)
  applyEventToAnswer e path eAns = passToFollowUps e path eAns

instance ApplyEventToAnswer DeleteQuestionEvent where
  applyEventToAnswer _ _ (Left error) = Left error
  applyEventToAnswer e (ansUuid:[]) (Right ans) = Right $ deleteFuQuestion ans (getEventQuestionUuid e)
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

-- -------------------
-- TAGS --------------
-- -------------------
instance ApplyEventToAnswer AddTagEvent where
  applyEventToAnswer e _ _ = errorIllegalState e "AddTagEvent" "Answer"

instance ApplyEventToAnswer EditTagEvent where
  applyEventToAnswer e _ _ = errorIllegalState e "EditTagEvent" "Answer"

instance ApplyEventToAnswer DeleteTagEvent where
  applyEventToAnswer = broadcastToFollowUps

-- -------------------
-- INTEGRATIONS ------
-- -------------------
instance ApplyEventToAnswer AddIntegrationEvent where
  applyEventToAnswer e _ _ = errorIllegalState e "AddIntegrationEvent" "Answer"

instance ApplyEventToAnswer EditIntegrationEvent where
  applyEventToAnswer = broadcastToFollowUps

instance ApplyEventToAnswer DeleteIntegrationEvent where
  applyEventToAnswer = broadcastToFollowUps

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

-- -------------------
-- TAGS --------------
-- -------------------
instance ApplyEventToExpert AddTagEvent where
  applyEventToExpert e _ _ = errorIllegalState e "AddTagEvent" "Expert"

instance ApplyEventToExpert EditTagEvent where
  applyEventToExpert e _ _ = errorIllegalState e "EditTagEvent" "Expert"

instance ApplyEventToExpert DeleteTagEvent where
  applyEventToExpert e _ _ = errorIllegalState e "DeleteTagEvent" "Expert"

-- -------------------
-- INTEGRATIONS ------
-- -------------------
instance ApplyEventToExpert AddIntegrationEvent where
  applyEventToExpert e _ _ = errorIllegalState e "AddIntegrationEvent" "Expert"

instance ApplyEventToExpert EditIntegrationEvent where
  applyEventToExpert e _ _ = errorIllegalState e "EditIntegrationEvent" "Expert"

instance ApplyEventToExpert DeleteIntegrationEvent where
  applyEventToExpert e _ _ = errorIllegalState e "DeleteIntegrationEvent" "Expert"

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

-- -------------------
-- TAGS --------------
-- -------------------
instance ApplyEventToReference AddTagEvent where
  applyEventToReference e _ _ = errorIllegalState e "AddTagEvent" "Reference"

instance ApplyEventToReference EditTagEvent where
  applyEventToReference e _ _ = errorIllegalState e "EditTagEvent" "Reference"

instance ApplyEventToReference DeleteTagEvent where
  applyEventToReference e _ _ = errorIllegalState e "DeleteTagEvent" "Reference"

-- -------------------
-- INTEGRATIONS ------
-- -------------------
instance ApplyEventToReference AddIntegrationEvent where
  applyEventToReference e _ _ = errorIllegalState e "AddIntegrationEvent" "Reference"

instance ApplyEventToReference EditIntegrationEvent where
  applyEventToReference e _ _ = errorIllegalState e "EditIntegrationEvent" "Reference"

instance ApplyEventToReference DeleteIntegrationEvent where
  applyEventToReference e _ _ = errorIllegalState e "DeleteIntegrationEvent" "Reference"

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO TAGS
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToTag AddKnowledgeModelEvent where
  applyEventToTag e _ _ = errorIllegalState e "AddKnowledgeModelEvent" "Tag"

instance ApplyEventToTag EditKnowledgeModelEvent where
  applyEventToTag e _ _ = errorIllegalState e "EditKnowledgeModelEvent" "Tag"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToTag AddChapterEvent where
  applyEventToTag e _ _ = errorIllegalState e "AddChapterEvent" "Tag"

instance ApplyEventToTag EditChapterEvent where
  applyEventToTag e _ _ = errorIllegalState e "EditChapterEvent" "Tag"

instance ApplyEventToTag DeleteChapterEvent where
  applyEventToTag e _ _ = errorIllegalState e "DeleteChapterEvent" "Tag"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToTag AddQuestionEvent where
  applyEventToTag e _ _ = errorIllegalState e "AddQuestionEvent" "Tag"

instance ApplyEventToTag EditQuestionEvent where
  applyEventToTag e _ _ = errorIllegalState e "EditQuestionEvent" "Tag"

instance ApplyEventToTag DeleteQuestionEvent where
  applyEventToTag e _ _ = errorIllegalState e "DeleteQuestionEvent" "Tag"

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToTag AddAnswerEvent where
  applyEventToTag e _ _ = errorIllegalState e "AddAnswerEvent" "Tag"

instance ApplyEventToTag EditAnswerEvent where
  applyEventToTag e _ _ = errorIllegalState e "EditAnswerEvent" "Tag"

instance ApplyEventToTag DeleteAnswerEvent where
  applyEventToTag e _ _ = errorIllegalState e "DeleteAnswerEvent" "Tag"

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToTag AddExpertEvent where
  applyEventToTag e _ _ = errorIllegalState e "AddExpertEvent" "Tag"

instance ApplyEventToTag EditExpertEvent where
  applyEventToTag e _ _ = errorIllegalState e "EditExpertEvent" "Tag"

instance ApplyEventToTag DeleteExpertEvent where
  applyEventToTag e _ _ = errorIllegalState e "DeleteExpertEvent" "Tag"

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToTag AddReferenceEvent where
  applyEventToTag e _ _ = errorIllegalState e "AddReferenceEvent" "Tag"

instance ApplyEventToTag EditReferenceEvent where
  applyEventToTag e _ _ = errorIllegalState e "EditReferenceEvent" "Tag"

instance ApplyEventToTag DeleteReferenceEvent where
  applyEventToTag e _ _ = errorIllegalState e "DeleteReferenceEvent" "Tag"

-- -------------------
-- TAGS --------------
-- -------------------
instance ApplyEventToTag AddTagEvent where
  applyEventToTag e _ _ = errorIllegalState e "AddTagEvent" "Expert"

instance ApplyEventToTag EditTagEvent where
  applyEventToTag _ _ (Left error) = Left error
  applyEventToTag e [] (Right tag) = Right $ editTag e tag
  applyEventToTag e path _ = errorPathShouldBeEmpty e path

instance ApplyEventToTag DeleteTagEvent where
  applyEventToTag e _ _ = errorIllegalState e "DeleteTagEvent" "Expert"

-- -------------------
-- INTEGRATIONS ------
-- -------------------
instance ApplyEventToTag AddIntegrationEvent where
  applyEventToTag e _ _ = errorIllegalState e "AddIntegrationEvent" "Tag"

instance ApplyEventToTag EditIntegrationEvent where
  applyEventToTag e _ _ = errorIllegalState e "EditIntegrationEvent" "Tag"

instance ApplyEventToTag DeleteIntegrationEvent where
  applyEventToTag e _ _ = errorIllegalState e "DeleteIntegrationEvent" "Tag"

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- APPLY TO INTEGRATIONS
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance ApplyEventToIntegration AddKnowledgeModelEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "AddKnowledgeModelEvent" "Integration"

instance ApplyEventToIntegration EditKnowledgeModelEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "EditKnowledgeModelEvent" "Integration"

-- -------------------
-- CHAPTERS ----------
-- -------------------
instance ApplyEventToIntegration AddChapterEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "AddChapterEvent" "Integration"

instance ApplyEventToIntegration EditChapterEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "EditChapterEvent" "Integration"

instance ApplyEventToIntegration DeleteChapterEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "DeleteChapterEvent" "Integration"

-- -------------------
-- QUESTIONS----------
-- -------------------
instance ApplyEventToIntegration AddQuestionEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "AddQuestionEvent" "Integration"

instance ApplyEventToIntegration EditQuestionEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "EditQuestionEvent" "Integration"

instance ApplyEventToIntegration DeleteQuestionEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "DeleteQuestionEvent" "Integration"

-- -------------------
-- ANSWERS -----------
-- -------------------
instance ApplyEventToIntegration AddAnswerEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "AddAnswerEvent" "Integration"

instance ApplyEventToIntegration EditAnswerEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "EditAnswerEvent" "Integration"

instance ApplyEventToIntegration DeleteAnswerEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "DeleteAnswerEvent" "Integration"

-- -------------------
-- EXPERTS -----------
-- -------------------
instance ApplyEventToIntegration AddExpertEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "AddExpertEvent" "Integration"

instance ApplyEventToIntegration EditExpertEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "EditExpertEvent" "Integration"

instance ApplyEventToIntegration DeleteExpertEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "DeleteExpertEvent" "Integration"

-- -------------------
-- REFERENCES---------
-- -------------------
instance ApplyEventToIntegration AddReferenceEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "AddReferenceEvent" "Integration"

instance ApplyEventToIntegration EditReferenceEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "EditReferenceEvent" "Integration"

instance ApplyEventToIntegration DeleteReferenceEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "DeleteReferenceEvent" "Integration"

-- -------------------
-- TAGS --------------
-- -------------------
instance ApplyEventToIntegration AddTagEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "AddTagEvent" "Integration"

instance ApplyEventToIntegration EditTagEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "EditTagEvent" "Integration"

instance ApplyEventToIntegration DeleteTagEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "DeleteTagEvent" "Integration"

-- -------------------
-- INTEGRATIONS ------
-- -------------------
instance ApplyEventToIntegration AddIntegrationEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "AddIntegrationEvent" "Expert"

instance ApplyEventToIntegration EditIntegrationEvent where
  applyEventToIntegration _ _ (Left error) = Left error
  applyEventToIntegration e [] (Right integration) = Right $ editIntegration e integration
  applyEventToIntegration e path _ = errorPathShouldBeEmpty e path

instance ApplyEventToIntegration DeleteIntegrationEvent where
  applyEventToIntegration e _ _ = errorIllegalState e "DeleteIntegrationEvent" "Expert"
