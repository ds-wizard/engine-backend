module Model.KnowledgeModel.KnowledgeModelAccessors where

import Control.Lens
import Data.List
import Data.Maybe (fromMaybe)
import Data.UUID

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

getChapterUuids :: KnowledgeModel -> [UUID]
getChapterUuids km = km ^.. chapters . traverse . uuid

kmChangeChapterUuidsOrder :: ([Chapter] -> Identity [UUID]) -> KnowledgeModel -> Identity KnowledgeModel
kmChangeChapterUuidsOrder convert km = Identity $ km & chapters .~ orderedChapters
  where
    ids :: Identity [UUID]
    ids = convert (km ^. chapters)
    orderedChapters :: [Chapter]
    orderedChapters = concatMap getChapterByUuid (runIdentity ids)
    getChapterByUuid :: UUID -> [Chapter]
    getChapterByUuid chUuid = filter (\x -> x ^. uuid == chUuid) (km ^. chapters)

kmChangeTagUuidsOrder :: ([Tag] -> Identity [UUID]) -> KnowledgeModel -> Identity KnowledgeModel
kmChangeTagUuidsOrder convert km = Identity $ km & tags .~ orderedTags
  where
    ids :: Identity [UUID]
    ids = convert (km ^. tags)
    orderedTags :: [Tag]
    orderedTags = concatMap getTagByUuid (runIdentity ids)
    getTagByUuid :: UUID -> [Tag]
    getTagByUuid tUuid = filter (\x -> x ^. uuid == tUuid) (km ^. tags)

getAllChapters :: KnowledgeModel -> [Chapter]
getAllChapters km = km ^. chapters

getChapterByUuid :: KnowledgeModel -> UUID -> Maybe Chapter
getChapterByUuid km chapterUuid = find (\ch -> ch ^. uuid == chapterUuid) (getAllChapters km)

isThereAnyChapterWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyChapterWithGivenUuid km chUuid = chUuid `elem` (getChapterUuid <$> getAllChapters km)
  where
    getChapterUuid chapter = chapter ^. uuid

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
getQuestionUuids :: Chapter -> [UUID]
getQuestionUuids ch = ch ^.. questions . traverse . uuid

chChangeQuestionUuidsOrder :: ([Question] -> Identity [UUID]) -> Chapter -> Identity Chapter
chChangeQuestionUuidsOrder convert ch = Identity $ ch & questions .~ orderedQuestions
  where
    ids :: Identity [UUID]
    ids = convert (ch ^. questions)
    orderedQuestions :: [Question]
    orderedQuestions = concatMap getQuestionByUuid (runIdentity ids)
    getQuestionByUuid :: UUID -> [Question]
    getQuestionByUuid qUuid = filter (\x -> x ^. uuid == qUuid) (ch ^. questions)

getAllQuestions :: KnowledgeModel -> [Question]
getAllQuestions km = go (km ^.. chapters . traverse . questions . traverse)
  where
    go :: [Question] -> [Question]
    go [] = []
    go questions = questions ++ (go . concat $ getNestedQuestions <$> questions)
    getNestedQuestions :: Question -> [Question]
    getNestedQuestions Question {_questionAnswerItemTemplate = (Just ait)} = ait ^. questions
    getNestedQuestions Question {_questionAnswers = (Just answers)} = concat $ _answerFollowUps <$> answers
    getNestedQuestions Question {_questionAnswers = Nothing} = []

getQuestionByUuid :: KnowledgeModel -> UUID -> Maybe Question
getQuestionByUuid km questionUuid = find (\q -> q ^. uuid == questionUuid) (getAllQuestions km)

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
  where
    getQuestionUuid question = question ^. uuid

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
getAnwerUuids :: Question -> Maybe [UUID]
getAnwerUuids Question {_questionAnswers = Just answers} = Just $ answers ^.. traverse . uuid
getAnwerUuids Question {_questionAnswers = Nothing} = Nothing

qChangeAnwerUuidsOrder :: (Maybe [Answer] -> Identity (Maybe [UUID])) -> Question -> Identity Question
qChangeAnwerUuidsOrder convert q = Identity $ q & answers .~ orderedAnwers
  where
    ids :: Identity (Maybe [UUID])
    ids = convert (q ^. answers)
    orderedAnwers :: Maybe [Answer]
    orderedAnwers =
      case (runIdentity ids) of
        Just uuids -> Just $ concatMap getAnswerByUuid uuids
        Nothing -> (q ^. answers)
    getAnswerByUuid :: UUID -> [Answer]
    getAnswerByUuid ansUuid =
      case q ^. answers of
        Just as -> filter (\x -> x ^. uuid == ansUuid) as
        Nothing -> []

getAllAnswers :: KnowledgeModel -> [Answer]
getAllAnswers km = concat $ getAnswer <$> getAllQuestions km
  where
    getAnswer :: Question -> [Answer]
    getAnswer question = fromMaybe [] (question ^. answers)

getAnswerByUuid :: KnowledgeModel -> UUID -> Maybe Answer
getAnswerByUuid km answerUuid = find (\ans -> ans ^. uuid == answerUuid) (getAllAnswers km)

getAllAnswersForQuestionUuid :: KnowledgeModel -> UUID -> [Answer]
getAllAnswersForQuestionUuid km questionUuid =
  case getQuestionByUuid km questionUuid of
    Just question -> fromMaybe [] (question ^. answers)
    Nothing -> []

isThereAnyAnswerWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyAnswerWithGivenUuid km ansUuid = ansUuid `elem` (getAnswerUuid <$> getAllAnswers km)
  where
    getAnswerUuid answer = answer ^. uuid

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
getAitQuestionUuids :: AnswerItemTemplate -> [UUID]
getAitQuestionUuids ait = ait ^.. questions . traverse . uuid

aitAnswerItemTemplatePlainWithUuids ::
     (Maybe AnswerItemTemplate -> Identity (Maybe AnswerItemTemplatePlainWithUuids)) -> Question -> Identity Question
aitAnswerItemTemplatePlainWithUuids convert q =
  case maybeAitPlainWithUuids of
    Just aitPlainWithUuids -> Identity $ q & answerItemTemplate .~ changedAnswerItemTemplate aitPlainWithUuids
    Nothing -> Identity q
  where
    maybeAitPlainWithUuids :: Maybe AnswerItemTemplatePlainWithUuids
    maybeAitPlainWithUuids = runIdentity $ convert (q ^. answerItemTemplate)
    changedAnswerItemTemplate :: AnswerItemTemplatePlainWithUuids -> Maybe AnswerItemTemplate
    changedAnswerItemTemplate aitPlainWithUuids =
      case q ^. answerItemTemplate of
        Just ait ->
          Just $ (ait & title .~ (aitPlainWithUuids ^. title)) & aitChangeAitQuestionUuidsOrder .~
          (aitPlainWithUuids ^. questionUuids)
        Nothing ->
          Just
            AnswerItemTemplate
            {_answerItemTemplateTitle = aitPlainWithUuids ^. title, _answerItemTemplateQuestions = []}

aitChangeAitQuestionUuidsOrder :: ([Question] -> Identity [UUID]) -> AnswerItemTemplate -> Identity AnswerItemTemplate
aitChangeAitQuestionUuidsOrder convert ait = Identity $ ait & questions .~ orderedQuestions
  where
    ids :: Identity [UUID]
    ids = convert (ait ^. questions)
    orderedQuestions :: [Question]
    orderedQuestions = concatMap getAitQuestionByUuid (runIdentity ids)
    getAitQuestionByUuid :: UUID -> [Question]
    getAitQuestionByUuid qUuid = filter (\x -> x ^. uuid == qUuid) (ait ^. questions)

getAllAnswerItemTemplates :: KnowledgeModel -> [AnswerItemTemplate]
getAllAnswerItemTemplates km = concat $ getAllAnswerItemTemplate <$> getAllQuestions km
  where
    getAllAnswerItemTemplate :: Question -> [AnswerItemTemplate]
    getAllAnswerItemTemplate question =
      case question ^. answerItemTemplate of
        Just ait -> [ait]
        Nothing -> []

getAllAitQuestionsForParentQuestionUuid :: KnowledgeModel -> UUID -> [Question]
getAllAitQuestionsForParentQuestionUuid km qUuid =
  case getQuestionByUuid km qUuid of
    Just q ->
      case q ^. answerItemTemplate of
        Just ait -> ait ^. questions
        Nothing -> []
    Nothing -> []

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
getFollowUpUuids :: Answer -> [UUID]
getFollowUpUuids ans = ans ^.. followUps . traverse . uuid

ansChangeFollowUpUuidsOrder :: ([Question] -> Identity [UUID]) -> Answer -> Identity Answer
ansChangeFollowUpUuidsOrder convert ans = Identity $ ans & followUps .~ orderedFollowUps
  where
    ids :: Identity [UUID]
    ids = convert (ans ^. followUps)
    orderedFollowUps :: [Question]
    orderedFollowUps = concatMap getFollowUpsByUuid (runIdentity ids)
    getFollowUpsByUuid :: UUID -> [Question]
    getFollowUpsByUuid fuqUuid = filter (\x -> x ^. uuid == fuqUuid) (ans ^. followUps)

------------------------------------------------------------------------------------------
getExpertUuids :: Question -> [UUID]
getExpertUuids q = q ^.. experts . traverse . uuid

qChangeExpertUuidsOrder :: ([Expert] -> Identity [UUID]) -> Question -> Identity Question
qChangeExpertUuidsOrder convert q = Identity $ q & experts .~ orderedExperts
  where
    ids :: Identity [UUID]
    ids = convert (q ^. experts)
    orderedExperts :: [Expert]
    orderedExperts = concatMap getExpertByUuid (runIdentity ids)
    getExpertByUuid :: UUID -> [Expert]
    getExpertByUuid expUuid = filter (\x -> x ^. uuid == expUuid) (q ^. experts)

getAllExperts :: KnowledgeModel -> [Expert]
getAllExperts km = concat $ getExpert <$> getAllQuestions km
  where
    getExpert :: Question -> [Expert]
    getExpert question = question ^. experts

getExpertByUuid :: KnowledgeModel -> UUID -> Maybe Expert
getExpertByUuid km expertUuid = find (\exp -> exp ^. uuid == expertUuid) (getAllExperts km)

getAllExpertsForQuestionUuid :: KnowledgeModel -> UUID -> [Expert]
getAllExpertsForQuestionUuid km questionUuid =
  case getQuestionByUuid km questionUuid of
    Just question -> question ^. experts
    Nothing -> []

isThereAnyExpertWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyExpertWithGivenUuid km expUuid = expUuid `elem` (getExpertUuid <$> getAllExperts km)
  where
    getExpertUuid expert = expert ^. uuid

------------------------------------------------------------------------------------------
getReferenceUuid :: Reference -> UUID
getReferenceUuid (ResourcePageReference' ref) = ref ^. uuid
getReferenceUuid (URLReference' ref) = ref ^. uuid
getReferenceUuid (CrossReference' ref) = ref ^. uuid

getReferenceUuids :: Question -> [UUID]
getReferenceUuids q = getReferenceUuid <$> (q ^. references)

qChangeReferenceUuidsOrder :: ([Reference] -> Identity [UUID]) -> Question -> Identity Question
qChangeReferenceUuidsOrder convert q = Identity $ q & references .~ orderedReferences
  where
    ids :: Identity [UUID]
    ids = convert (q ^. references)
    orderedReferences :: [Reference]
    orderedReferences = concatMap getReferenceByUuid (runIdentity ids)
    getReferenceByUuid :: UUID -> [Reference]
    getReferenceByUuid refUuid = filter (\x -> (getReferenceUuid x) == refUuid) (q ^. references)

getAllReferences :: KnowledgeModel -> [Reference]
getAllReferences km = concat $ getReference <$> getAllQuestions km
  where
    getReference :: Question -> [Reference]
    getReference question = question ^. references

getReferenceByUuid :: KnowledgeModel -> UUID -> Maybe Reference
getReferenceByUuid km refUuid = find (\ref -> (getReferenceUuid ref) == refUuid) (getAllReferences km)

getAllReferencesForQuestionUuid :: KnowledgeModel -> UUID -> [Reference]
getAllReferencesForQuestionUuid km questionUuid =
  case getQuestionByUuid km questionUuid of
    Just question -> question ^. references
    Nothing -> []

isThereAnyReferenceWithGivenUuid :: KnowledgeModel -> UUID -> Bool
isThereAnyReferenceWithGivenUuid km refUuid = refUuid `elem` (getReferenceUuid <$> getAllReferences km)

------------------------------------------------------------------------------------------
getTagUuids :: KnowledgeModel -> [UUID]
getTagUuids km = km ^.. tags . traverse . uuid
