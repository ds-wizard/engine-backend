module Model.KnowledgeModel.KnowledgeModelAccessors
  -- Chapter
  ( getAllChapters
  , getChapterByUuid
  , isThereAnyChapterWithGivenUuid
  -- Tag
  , getAllTags
  , getTagByUuid
  , isThereAnyTagWithGivenUuid
  -- Integration
  , getAllIntegrations
  , getIntegrationByUuid
  , isThereAnyIntegrationWithGivenUuid
  -- Question
  , getAllQuestions
  , getQuestionByUuid
  , getAllQuestionsForChapterUuid
  , getAllQuestionsForAnswerUuid
  , isThereAnyQuestionWithGivenUuid
  -- Expert
  , getAllExperts
  , getExpertByUuid
  , getAllExpertsForQuestionUuid
  , isThereAnyExpertWithGivenUuid
  -- Reference
  , getAllReferences
  , getReferenceByUuid
  , getAllReferencesForQuestionUuid
  , isThereAnyReferenceWithGivenUuid
  -- Answer
  , getAllAnswers
  , getAnswerByUuid
  , getAllAnswersForQuestionUuid
  , isThereAnyAnswerWithGivenUuid
  -- ItemTemplateQuestion
  , getAllItemTemplateQuestionsForQuestionUuid
  ) where

import Control.Lens
import Data.List
import Data.UUID

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses

-- -------------------
-- CHAPTERS ----------
-- -------------------
getAllChapters :: KnowledgeModel -> [Chapter]
getAllChapters km = km ^. chapters

getChapterByUuid :: KnowledgeModel -> UUID -> Maybe Chapter
getChapterByUuid km chapterUuid = find (\ch -> ch ^. uuid == chapterUuid) (getAllChapters km)

isThereAnyChapterWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyChapterWithGivenUuid km chUuid = chUuid `elem` (getChapterUuid <$> getAllChapters km)
  where
    getChapterUuid chapter = chapter ^. uuid

-- -------------------
-- TAGS --------------
-- -------------------
getAllTags :: KnowledgeModel -> [Tag]
getAllTags km = km ^. tags

getTagByUuid :: KnowledgeModel -> UUID -> Maybe Tag
getTagByUuid km tagUuid = find (\t -> t ^. uuid == tagUuid) (getAllTags km)

isThereAnyTagWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyTagWithGivenUuid km tagUuid = tagUuid `elem` (getTagByUuid <$> getAllTags km)
  where
    getTagByUuid tag = tag ^. uuid

-- -------------------
-- INTEGRATIONS ------
-- -------------------
getAllIntegrations :: KnowledgeModel -> [Integration]
getAllIntegrations km = km ^. integrations

getIntegrationByUuid :: KnowledgeModel -> UUID -> Maybe Integration
getIntegrationByUuid km integrationUuid = find (\i -> i ^. uuid == integrationUuid) (getAllIntegrations km)

isThereAnyIntegrationWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyIntegrationWithGivenUuid km integrationUuid =
  integrationUuid `elem` (getIntegrationByUuid <$> getAllIntegrations km)
  where
    getIntegrationByUuid integration = integration ^. uuid

-- -------------------
-- QUESTIONS----------
-- -------------------
getAllQuestions :: KnowledgeModel -> [Question]
getAllQuestions km = go (km ^.. chapters . traverse . questions . traverse)
  where
    go :: [Question] -> [Question]
    go [] = []
    go questions = questions ++ (go . concat $ getNestedQuestions <$> questions)
    getNestedQuestions :: Question -> [Question]
    getNestedQuestions (ListQuestion' q) = q ^. itemTemplateQuestions
    getNestedQuestions (OptionsQuestion' q) = concat $ _answerFollowUps <$> (q ^. answers)
    getNestedQuestions _ = []

getQuestionByUuid :: KnowledgeModel -> UUID -> Maybe Question
getQuestionByUuid km questionUuid = find (\q -> getQuestionUuid q == questionUuid) (getAllQuestions km)

getAllQuestionsForChapterUuid :: KnowledgeModel -> UUID -> [Question]
getAllQuestionsForChapterUuid km chapterUuid =
  case getChapterByUuid km chapterUuid of
    Just chapter -> chapter ^. questions
    Nothing -> []

getAllQuestionsForAnswerUuid :: KnowledgeModel -> UUID -> [Question]
getAllQuestionsForAnswerUuid km answerUuid =
  case getAnswerByUuid km answerUuid of
    Just answer -> answer ^. followUps
    Nothing -> []

isThereAnyQuestionWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyQuestionWithGivenUuid km qUuid = qUuid `elem` (getQuestionUuid <$> getAllQuestions km)

-- -------------------
-- EXPERT ------------
-- -------------------
getAllExperts :: KnowledgeModel -> [Expert]
getAllExperts km = concat $ getExperts <$> getAllQuestions km

getExpertByUuid :: KnowledgeModel -> UUID -> Maybe Expert
getExpertByUuid km expertUuid = find (\exp -> exp ^. uuid == expertUuid) (getAllExperts km)

getAllExpertsForQuestionUuid :: KnowledgeModel -> UUID -> [Expert]
getAllExpertsForQuestionUuid km questionUuid =
  case getQuestionByUuid km questionUuid of
    Just question -> getExperts question
    Nothing -> []

isThereAnyExpertWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyExpertWithGivenUuid km expUuid = expUuid `elem` (getExpertUuid <$> getAllExperts km)
  where
    getExpertUuid expert = expert ^. uuid

-- -------------------
-- REFERENCE ---------
-- -------------------
getAllReferences :: KnowledgeModel -> [Reference]
getAllReferences km = concat $ getReferences <$> getAllQuestions km

getReferenceByUuid :: KnowledgeModel -> UUID -> Maybe Reference
getReferenceByUuid km refUuid = find (\ref -> (getReferenceUuid ref) == refUuid) (getAllReferences km)

getAllReferencesForQuestionUuid :: KnowledgeModel -> UUID -> [Reference]
getAllReferencesForQuestionUuid km questionUuid =
  case getQuestionByUuid km questionUuid of
    Just question -> getReferences question
    Nothing -> []

isThereAnyReferenceWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyReferenceWithGivenUuid km refUuid = refUuid `elem` (getReferenceUuid <$> getAllReferences km)

-- -------------------
-- ANSWER ------------
-- -------------------
getAllAnswers :: KnowledgeModel -> [Answer]
getAllAnswers km = concat $ getAnswer <$> getAllQuestions km
  where
    getAnswer :: Question -> [Answer]
    getAnswer (OptionsQuestion' q) = q ^. answers
    getAnswer _ = []

getAnswerByUuid :: KnowledgeModel -> UUID -> Maybe Answer
getAnswerByUuid km answerUuid = find (\ans -> ans ^. uuid == answerUuid) (getAllAnswers km)

getAllAnswersForQuestionUuid :: KnowledgeModel -> UUID -> [Answer]
getAllAnswersForQuestionUuid km questionUuid =
  case getQuestionByUuid km questionUuid of
    Just (OptionsQuestion' q) -> q ^. answers
    _ -> []

isThereAnyAnswerWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyAnswerWithGivenUuid km ansUuid = ansUuid `elem` (getAnswerUuid <$> getAllAnswers km)
  where
    getAnswerUuid answer = answer ^. uuid

-- -------------------------------
-- ITEM TEMPLATE QUESTION --------
-------------------------------
getAllItemTemplateQuestionsForQuestionUuid :: KnowledgeModel -> UUID -> [Question]
getAllItemTemplateQuestionsForQuestionUuid km qUuid =
  case getQuestionByUuid km qUuid of
    Just (ListQuestion' q) -> q ^. itemTemplateQuestions
    _ -> []
