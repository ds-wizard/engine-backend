module Model.KnowledgeModel.KnowledgeModelLenses where

import Control.Lens
import Data.Map (Map, empty)
import Data.UUID

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
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

kmChangeIntegrationUuidsOrder :: ([Integration] -> Identity [UUID]) -> KnowledgeModel -> Identity KnowledgeModel
kmChangeIntegrationUuidsOrder convert km = Identity $ km & integrations .~ orderedIntegrations
  where
    ids :: Identity [UUID]
    ids = convert (km ^. integrations)
    orderedIntegrations :: [Integration]
    orderedIntegrations = concatMap getIntegrationByUuid (runIdentity ids)
    getIntegrationByUuid :: UUID -> [Integration]
    getIntegrationByUuid iUuid = filter (\x -> x ^. uuid == iUuid) (km ^. integrations)

-- -------------------
-- CHAPTERS ----------
-- -------------------
chChangeQuestionUuidsOrder :: ([Question] -> Identity [UUID]) -> Chapter -> Identity Chapter
chChangeQuestionUuidsOrder convert ch = Identity $ ch & questions .~ orderedQuestions
  where
    ids :: Identity [UUID]
    ids = convert (ch ^. questions)
    orderedQuestions :: [Question]
    orderedQuestions = concatMap getQuestionByUuid (runIdentity ids)
    getQuestionByUuid :: UUID -> [Question]
    getQuestionByUuid qUuid = filter (\x -> getQuestionUuid x == qUuid) (ch ^. questions)

-- -------------------
-- QUESTIONS----------
-- -------------------
getQuestionUuid :: Question -> UUID
getQuestionUuid (OptionsQuestion' q) = q ^. uuid
getQuestionUuid (ListQuestion' q) = q ^. uuid
getQuestionUuid (ValueQuestion' q) = q ^. uuid
getQuestionUuid (IntegrationQuestion' q) = q ^. uuid

------------------------------------------------------------------------------------------
qChangeTitle :: (String -> Identity String) -> Question -> Identity Question
qChangeTitle convert q = Identity . updateQuestion $ q
  where
    newValue :: String
    newValue = runIdentity . convert $ ""
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & title .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & title .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & title .~ newValue
    updateQuestion (IntegrationQuestion' q) = IntegrationQuestion' $ q & title .~ newValue

------------------------------------------------------------------------------------------
qChangeText :: (Maybe String -> Identity (Maybe String)) -> Question -> Identity Question
qChangeText convert q = Identity . updateQuestion $ q
  where
    newValue :: Maybe String
    newValue = runIdentity . convert $ Nothing
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & text .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & text .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & text .~ newValue
    updateQuestion (IntegrationQuestion' q) = IntegrationQuestion' $ q & text .~ newValue

------------------------------------------------------------------------------------------
qChangeRequiredLevel :: (Maybe Int -> Identity (Maybe Int)) -> Question -> Identity Question
qChangeRequiredLevel convert q = Identity . updateQuestion $ q
  where
    newValue :: Maybe Int
    newValue = runIdentity . convert $ Nothing
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & requiredLevel .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & requiredLevel .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & requiredLevel .~ newValue
    updateQuestion (IntegrationQuestion' q) = IntegrationQuestion' $ q & requiredLevel .~ newValue

------------------------------------------------------------------------------------------
getTagUuids :: Question -> [UUID]
getTagUuids (OptionsQuestion' q) = q ^. tagUuids
getTagUuids (ListQuestion' q) = q ^. tagUuids
getTagUuids (ValueQuestion' q) = q ^. tagUuids
getTagUuids (IntegrationQuestion' q) = q ^. tagUuids

qChangeTagUuids :: ([UUID] -> Identity [UUID]) -> Question -> Identity Question
qChangeTagUuids convert q = Identity . updateQuestion $ q
  where
    newValue :: [UUID]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & tagUuids .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & tagUuids .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & tagUuids .~ newValue
    updateQuestion (IntegrationQuestion' q) = IntegrationQuestion' $ q & tagUuids .~ newValue

------------------------------------------------------------------------------------------
getExpertUuids :: Question -> [UUID]
getExpertUuids q = (getExperts q) ^.. traverse . uuid

getExperts :: Question -> [Expert]
getExperts (OptionsQuestion' q) = q ^. experts
getExperts (ListQuestion' q) = q ^. experts
getExperts (ValueQuestion' q) = q ^. experts
getExperts (IntegrationQuestion' q) = q ^. experts

qChangeExperts :: ([Expert] -> Identity [Expert]) -> Question -> Identity Question
qChangeExperts convert q = Identity . updateQuestion $ q
  where
    newValue :: [Expert]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & experts .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & experts .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & experts .~ newValue
    updateQuestion (IntegrationQuestion' q) = IntegrationQuestion' $ q & experts .~ newValue

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
    updateQuestion (IntegrationQuestion' q) orderedExperts = IntegrationQuestion' $ q & experts .~ orderedExperts

------------------------------------------------------------------------------------------
getReferences :: Question -> [Reference]
getReferences (OptionsQuestion' q) = q ^. references
getReferences (ListQuestion' q) = q ^. references
getReferences (ValueQuestion' q) = q ^. references
getReferences (IntegrationQuestion' q) = q ^. references

getReferenceUuids :: Question -> [UUID]
getReferenceUuids q = getReferenceUuid <$> (getReferences q)

qChangeReferences :: ([Reference] -> Identity [Reference]) -> Question -> Identity Question
qChangeReferences convert q = Identity . updateQuestion $ q
  where
    newValue :: [Reference]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & references .~ newValue
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & references .~ newValue
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & references .~ newValue
    updateQuestion (IntegrationQuestion' q) = IntegrationQuestion' $ q & references .~ newValue

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
    updateQuestion (IntegrationQuestion' q) orderedReferences =
      IntegrationQuestion' $ q & references .~ orderedReferences

------------------------------------------------------------------------------------------
getAnswers :: Question -> [Answer]
getAnswers (OptionsQuestion' q) = q ^. answers
getAnswers q = []

getAnwerUuids :: OptionsQuestion -> [UUID]
getAnwerUuids q = q ^. answers ^.. traverse . uuid

qChangeAnswers :: ([Answer] -> Identity [Answer]) -> Question -> Identity Question
qChangeAnswers convert q = Identity . updateQuestion $ q
  where
    newValue :: [Answer]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (OptionsQuestion' q) = OptionsQuestion' $ q & answers .~ newValue
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

------------------------------------------------------------------------------------------
getItemTemplateQuestions :: Question -> [Question]
getItemTemplateQuestions (ListQuestion' q) = q ^. itemTemplateQuestions
getItemTemplateQuestions q = []

qChangeItemTemplateQuestions :: ([Question] -> Identity [Question]) -> Question -> Identity Question
qChangeItemTemplateQuestions convert q = Identity . updateQuestion $ q
  where
    newValue :: [Question]
    newValue = runIdentity . convert $ []
    updateQuestion :: Question -> Question
    updateQuestion (ListQuestion' q) = ListQuestion' $ q & itemTemplateQuestions .~ newValue
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

------------------------------------------------------------------------------------------
qChangeValueType :: (QuestionValueType -> Identity QuestionValueType) -> Question -> Identity Question
qChangeValueType convert = Identity . updateQuestion
  where
    newValue :: QuestionValueType
    newValue = runIdentity . convert $ StringQuestionValueType
    updateQuestion :: Question -> Question
    updateQuestion (ValueQuestion' q) = ValueQuestion' $ q & valueType .~ newValue
    updateQuestion q = q

------------------------------------------------------------------------------------------
qChangeIntegrationUuid :: (UUID -> Identity UUID) -> Question -> Identity Question
qChangeIntegrationUuid convert = Identity . updateQuestion
  where
    newValue :: UUID
    newValue = runIdentity . convert $ nil
    updateQuestion :: Question -> Question
    updateQuestion (IntegrationQuestion' q) = IntegrationQuestion' $ q & integrationUuid .~ newValue
    updateQuestion q = q

qChangeProps :: (Map String String -> Identity (Map String String)) -> Question -> Identity Question
qChangeProps convert = Identity . updateQuestion
  where
    newValue :: Map String String
    newValue = runIdentity . convert $ empty
    updateQuestion :: Question -> Question
    updateQuestion (IntegrationQuestion' q) = IntegrationQuestion' $ q & props .~ newValue
    updateQuestion q = q

-- -------------------
-- REFERENCE ---------
-- -------------------
getReferenceUuid :: Reference -> UUID
getReferenceUuid (ResourcePageReference' ref) = ref ^. uuid
getReferenceUuid (URLReference' ref) = ref ^. uuid
getReferenceUuid (CrossReference' ref) = ref ^. uuid

-- -------------------
-- ANSWER ------------
-- -------------------
ansChangeFollowUpUuidsOrder :: ([Question] -> Identity [UUID]) -> Answer -> Identity Answer
ansChangeFollowUpUuidsOrder convert ans = Identity $ ans & followUps .~ orderedFollowUps
  where
    ids :: Identity [UUID]
    ids = convert (ans ^. followUps)
    orderedFollowUps :: [Question]
    orderedFollowUps = concatMap getFollowUpsByUuid (runIdentity ids)
    getFollowUpsByUuid :: UUID -> [Question]
    getFollowUpsByUuid fuqUuid = filter (\x -> getQuestionUuid x == fuqUuid) (ans ^. followUps)
