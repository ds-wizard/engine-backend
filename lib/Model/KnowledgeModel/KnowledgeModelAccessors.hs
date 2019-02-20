module Model.KnowledgeModel.KnowledgeModelAccessors where

import Control.Lens
import Data.List
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
getQuestionUuids ch = getQuestionUuid <$> ch ^. questions

chChangeQuestionUuidsOrder :: ([Question] -> Identity [UUID]) -> Chapter -> Identity Chapter
chChangeQuestionUuidsOrder convert ch = Identity $ ch & questions .~ orderedQuestions
  where
    ids :: Identity [UUID]
    ids = convert (ch ^. questions)
    orderedQuestions :: [Question]
    orderedQuestions = concatMap getQuestionByUuid (runIdentity ids)
    getQuestionByUuid :: UUID -> [Question]
    getQuestionByUuid qUuid = filter (\x -> getQuestionUuid x == qUuid) (ch ^. questions)

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

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
getQuestionUuid :: Question -> UUID
getQuestionUuid (OptionsQuestion' q) = q ^. uuid
getQuestionUuid (ListQuestion' q) = q ^. uuid
getQuestionUuid (ValueQuestion' q) = q ^. uuid

getTagUuids :: Question -> [UUID]
getTagUuids (OptionsQuestion' q) = q ^. tagUuids
getTagUuids (ListQuestion' q) = q ^. tagUuids
getTagUuids (ValueQuestion' q) = q ^. tagUuids

getAnwerUuids :: OptionsQuestion -> [UUID]
getAnwerUuids q = q ^. answers ^.. traverse . uuid

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

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
getAllItQuestionsForParentQuestionUuid :: KnowledgeModel -> UUID -> [Question]
getAllItQuestionsForParentQuestionUuid km qUuid =
  case getQuestionByUuid km qUuid of
    Just (ListQuestion' q) -> q ^. itemTemplateQuestions
    _ -> []

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
getFollowUpUuids :: Answer -> [UUID]
getFollowUpUuids ans = getQuestionUuid <$> (ans ^. followUps)

ansChangeFollowUpUuidsOrder :: ([Question] -> Identity [UUID]) -> Answer -> Identity Answer
ansChangeFollowUpUuidsOrder convert ans = Identity $ ans & followUps .~ orderedFollowUps
  where
    ids :: Identity [UUID]
    ids = convert (ans ^. followUps)
    orderedFollowUps :: [Question]
    orderedFollowUps = concatMap getFollowUpsByUuid (runIdentity ids)
    getFollowUpsByUuid :: UUID -> [Question]
    getFollowUpsByUuid fuqUuid = filter (\x -> getQuestionUuid x == fuqUuid) (ans ^. followUps)

------------------------------------------------------------------------------------------
getExpertUuids :: Question -> [UUID]
getExpertUuids q = (getExperts q) ^.. traverse . uuid

getExperts :: Question -> [Expert]
getExperts (OptionsQuestion' q) = q ^. experts
getExperts (ListQuestion' q) = q ^. experts
getExperts (ValueQuestion' q) = q ^. experts

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

------------------------------------------------------------------------------------------
getReferenceUuid :: Reference -> UUID
getReferenceUuid (ResourcePageReference' ref) = ref ^. uuid
getReferenceUuid (URLReference' ref) = ref ^. uuid
getReferenceUuid (CrossReference' ref) = ref ^. uuid

getReferences :: Question -> [Reference]
getReferences (OptionsQuestion' q) = q ^. references
getReferences (ListQuestion' q) = q ^. references
getReferences (ValueQuestion' q) = q ^. references

getReferenceUuids :: Question -> [UUID]
getReferenceUuids q = getReferenceUuid <$> (getReferences q)

getAnswers :: Question -> [Answer]
getAnswers (OptionsQuestion' q) = q ^. answers
getAnswers q = []

getItemTemplateQuestions :: Question -> [Question]
getItemTemplateQuestions (ListQuestion' q) = q ^. itemTemplateQuestions
getItemTemplateQuestions q = []

qChangeTitle :: (String -> Identity String) -> Question -> Identity Question
qChangeTitle convert q = Identity . updateQuestion $ q
  where
    newValue :: String
    newValue = runIdentity . convert $ ""
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & title .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & title .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & title .~ newValue

qChangeText :: (Maybe String -> Identity (Maybe String)) -> Question -> Identity Question
qChangeText convert q = Identity . updateQuestion $ q
  where
    newValue :: Maybe String
    newValue = runIdentity . convert $ Nothing
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & text .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & text .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & text .~ newValue

qChangeRequiredLevel :: (Maybe Int -> Identity (Maybe Int)) -> Question -> Identity Question
qChangeRequiredLevel convert q = Identity . updateQuestion $ q
  where
    newValue :: Maybe Int
    newValue = runIdentity . convert $ Nothing
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & requiredLevel .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & requiredLevel .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & requiredLevel .~ newValue

qChangeTagUuids :: ([UUID] -> Identity [UUID]) -> Question -> Identity Question
qChangeTagUuids convert q = Identity . updateQuestion $ q
  where
    newValue :: [UUID]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & tagUuids .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & tagUuids .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & tagUuids .~ newValue

qChangeExperts :: ([Expert] -> Identity [Expert]) -> Question -> Identity Question
qChangeExperts convert q = Identity . updateQuestion $ q
  where
    newValue :: [Expert]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & experts .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & experts .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & experts .~ newValue

qChangeExpertUuidsOrder :: ([Expert] -> Identity [UUID]) -> Question -> Identity Question
qChangeExpertUuidsOrder convert q = Identity . updateQuestion q $ orderedExperts
  where
    ids :: Identity [UUID]
    ids = convert . getExperts $ q
    orderedExperts :: [Expert]
    orderedExperts = concatMap getExpertByUuid (runIdentity ids)
    getExpertByUuid :: UUID -> [Expert]
    getExpertByUuid expUuid = filter (\x -> x ^. uuid == expUuid) (getExperts q)
    updateQuestion :: Question -> [Expert] -> Question
    updateQuestion (OptionsQuestion' q) orderedExperts = OptionsQuestion' $ q & experts .~ orderedExperts
    updateQuestion (ListQuestion' q) orderedExperts = ListQuestion' $ q & experts .~ orderedExperts
    updateQuestion (ValueQuestion' q) orderedExperts = ValueQuestion' $ q & experts .~ orderedExperts

qChangeReferences :: ([Reference] -> Identity [Reference]) -> Question -> Identity Question
qChangeReferences convert q = Identity . updateQuestion $ q
  where
    newValue :: [Reference]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & references .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & references .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & references .~ newValue

qChangeReferenceUuidsOrder :: ([Reference] -> Identity [UUID]) -> Question -> Identity Question
qChangeReferenceUuidsOrder convert q = Identity . updateQuestion q $ orderedReferences
  where
    ids :: Identity [UUID]
    ids = convert . getReferences $ q
    orderedReferences :: [Reference]
    orderedReferences = concatMap getReferenceByUuid (runIdentity ids)
    getReferenceByUuid :: UUID -> [Reference]
    getReferenceByUuid refUuid = filter (\x -> (getReferenceUuid x) == refUuid) (getReferences q)
    updateQuestion :: Question -> [Reference] -> Question
    updateQuestion (OptionsQuestion' q) orderedReferences = OptionsQuestion' $ q & references .~ orderedReferences
    updateQuestion (ListQuestion' q) orderedReferences = ListQuestion' $ q & references .~ orderedReferences
    updateQuestion (ValueQuestion' q) orderedReferences = ValueQuestion' $ q & references .~ orderedReferences

qChangeAnswers :: ([Answer] -> Identity [Answer]) -> Question -> Identity Question
qChangeAnswers convert q = Identity . updateQuestion $ q
  where
    newValue :: [Answer]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & answers .~ newValue
    updateQuestion q = q

qChangeItemTemplateQuestions :: ([Question] -> Identity [Question]) -> Question -> Identity Question
qChangeItemTemplateQuestions convert q = Identity . updateQuestion $ q
  where
    newValue :: [Question]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & itemTemplateQuestions .~ newValue
    updateQuestion q = q

qChangeAnwerUuidsOrder :: ([Answer] -> Identity [UUID]) -> Question -> Identity Question
qChangeAnwerUuidsOrder convert q = Identity . updateQuestion $ q
  where
    ids :: [UUID]
    ids = runIdentity . convert . getAnswers $ q
    orderedAnwers :: [Answer]
    orderedAnwers = concatMap getAnswerByUuid ids
    getAnswerByUuid :: UUID -> [Answer]
    getAnswerByUuid ansUuid = filter (\x -> x ^. uuid == ansUuid) (getAnswers q)
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & answers .~ orderedAnwers
    updateQuestion q = q

qChangeItemTemplateTitle :: (String -> Identity String) -> Question -> Identity Question
qChangeItemTemplateTitle convert q = Identity . updateQuestion $ q
  where
    newValue :: String
    newValue = runIdentity . convert $ ""
    updateQuestion :: Question -> Question
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & itemTemplateTitle .~ newValue
    updateQuestion q = q

qChangeItemTemplateQuestionUuidsOrder :: ([Question] -> Identity [UUID]) -> Question -> Identity Question
qChangeItemTemplateQuestionUuidsOrder convert q = Identity . updateQuestion $ q
  where
    ids :: [UUID]
    ids = runIdentity . convert . getItemTemplateQuestions $ q
    orderedQuestions :: [Question]
    orderedQuestions = concatMap getQuestionByUuid ids
    getQuestionByUuid :: UUID -> [Question]
    getQuestionByUuid qUuid = filter (\x -> getQuestionUuid x == qUuid) (getItemTemplateQuestions q)
    updateQuestion :: Question -> Question
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & itemTemplateQuestions .~ orderedQuestions
    updateQuestion q = q

qChangeValueType :: (QuestionValueType -> Identity QuestionValueType) -> Question -> Identity Question
qChangeValueType convert = Identity . updateQuestion
  where
    newValue :: QuestionValueType
    newValue = runIdentity . convert $ StringQuestionValueType
    updateQuestion :: Question -> Question
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & valueType .~ newValue
    updateQuestion q = q

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
