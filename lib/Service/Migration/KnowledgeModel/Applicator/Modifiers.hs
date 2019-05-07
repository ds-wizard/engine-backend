module Service.Migration.KnowledgeModel.Applicator.Modifiers where

import Control.Lens ((&), (.~), (^.))
import Data.Map (empty, fromList, lookup)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Prelude hiding (lookup)

import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses

applyValue (ChangedValue val) ch setter = ch & setter .~ val
applyValue NothingChanged ch setter = ch

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
createKM :: AddKnowledgeModelEvent -> KnowledgeModel
createKM e =
  KnowledgeModel
  { _knowledgeModelUuid = e ^. kmUuid
  , _knowledgeModelName = e ^. name
  , _knowledgeModelChapters = []
  , _knowledgeModelTags = []
  , _knowledgeModelIntegrations = []
  }

editKM :: EditKnowledgeModelEvent -> KnowledgeModel -> KnowledgeModel
editKM e = applyIntegrationUuids . applyTagUuids . applyChapterUuids . applyName
  where
    applyName km = applyValue (e ^. name) km name
    applyChapterUuids km = applyValue (e ^. chapterUuids) km kmChangeChapterUuidsOrder
    applyTagUuids km = applyValue (e ^. tagUuids) km kmChangeTagUuidsOrder
    applyIntegrationUuids km = applyValue (e ^. integrationUuids) km kmChangeIntegrationUuidsOrder

-- -------------------
addChapter :: KnowledgeModel -> Chapter -> KnowledgeModel
addChapter km ch = km & chapters .~ (km ^. chapters ++ [ch])

deleteChapter :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteChapter km chUuid = km & chapters .~ (filter (\ch -> ch ^. uuid /= chUuid) (km ^. chapters))

-- -------------------
addTag :: KnowledgeModel -> Tag -> KnowledgeModel
addTag km t = km & tags .~ (km ^. tags ++ [t])

deleteTag :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteTag km tUuid = km & tags .~ (filter (\t -> t ^. uuid /= tUuid) (km ^. tags))

-- -------------------
addIntegration :: KnowledgeModel -> Integration -> KnowledgeModel
addIntegration km i = km & integrations .~ (km ^. integrations ++ [i])

deleteIntegration :: KnowledgeModel -> U.UUID -> KnowledgeModel
deleteIntegration km iUuid = km & integrations .~ (filter (\i -> i ^. uuid /= iUuid) (km ^. integrations))

-- -------------------
-- CHAPTERS ----------
-- -------------------
createChapter :: AddChapterEvent -> Chapter
createChapter e =
  Chapter
  {_chapterUuid = e ^. chapterUuid, _chapterTitle = e ^. title, _chapterText = e ^. text, _chapterQuestions = []}

editChapter :: EditChapterEvent -> Chapter -> Chapter
editChapter e = applyQuestionUuids . applyText . applyTitle
  where
    applyTitle ch = applyValue (e ^. title) ch title
    applyText ch = applyValue (e ^. text) ch text
    applyQuestionUuids ch = applyValue (e ^. questionUuids) ch chChangeQuestionUuidsOrder

-- -------------------
addQuestion :: Chapter -> Question -> Chapter
addQuestion ch q = ch & questions .~ (ch ^. questions ++ [q])

deleteQuestion :: Chapter -> U.UUID -> Chapter
deleteQuestion ch qUuid = ch & questions .~ (filter (\q -> getQuestionUuid q /= qUuid) (ch ^. questions))

-- -------------------
-- QUESTIONS----------
-- -------------------
createQuestion :: AddQuestionEvent -> Question
createQuestion (AddOptionsQuestionEvent' e) =
  OptionsQuestion' $
  OptionsQuestion
  { _optionsQuestionUuid = e ^. questionUuid
  , _optionsQuestionTitle = e ^. title
  , _optionsQuestionText = e ^. text
  , _optionsQuestionRequiredLevel = e ^. requiredLevel
  , _optionsQuestionTagUuids = e ^. tagUuids
  , _optionsQuestionReferences = []
  , _optionsQuestionExperts = []
  , _optionsQuestionAnswers = []
  }
createQuestion (AddListQuestionEvent' e) =
  ListQuestion' $
  ListQuestion
  { _listQuestionUuid = e ^. questionUuid
  , _listQuestionTitle = e ^. title
  , _listQuestionText = e ^. text
  , _listQuestionRequiredLevel = e ^. requiredLevel
  , _listQuestionTagUuids = e ^. tagUuids
  , _listQuestionReferences = []
  , _listQuestionExperts = []
  , _listQuestionItemTemplateTitle = e ^. itemTemplateTitle
  , _listQuestionItemTemplateQuestions = []
  }
createQuestion (AddValueQuestionEvent' e) =
  ValueQuestion' $
  ValueQuestion
  { _valueQuestionUuid = e ^. questionUuid
  , _valueQuestionTitle = e ^. title
  , _valueQuestionText = e ^. text
  , _valueQuestionRequiredLevel = e ^. requiredLevel
  , _valueQuestionTagUuids = e ^. tagUuids
  , _valueQuestionReferences = []
  , _valueQuestionExperts = []
  , _valueQuestionValueType = e ^. valueType
  }
createQuestion (AddIntegrationQuestionEvent' e) =
  IntegrationQuestion' $
  IntegrationQuestion
  { _integrationQuestionUuid = e ^. questionUuid
  , _integrationQuestionTitle = e ^. title
  , _integrationQuestionText = e ^. text
  , _integrationQuestionRequiredLevel = e ^. requiredLevel
  , _integrationQuestionTagUuids = e ^. tagUuids
  , _integrationQuestionReferences = []
  , _integrationQuestionExperts = []
  , _integrationQuestionIntegrationUuid = e ^. integrationUuid
  , _integrationQuestionProps = e ^. props
  }

editQuestion :: EditQuestionEvent -> Question -> Question
editQuestion e' q =
  case e' of
    (EditOptionsQuestionEvent' e) -> applyToOptionsQuestion e . convertToOptionsQuestion $ q
    (EditListQuestionEvent' e) -> applyToListQuestion e . convertToListQuestion $ q
    (EditValueQuestionEvent' e) -> applyToValueQuestion e . convertToValueQuestion $ q
    (EditIntegrationQuestionEvent' e) -> applyToIntegrationQuestion e . convertToIntegrationQuestion $ q
  where
    applyToOptionsQuestion e =
      applyAnwerUuids e .
      applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
    applyToListQuestion e =
      applyItemTemplateQuestions e .
      applyItemTemplateTitle e .
      applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
    applyToValueQuestion e =
      applyValueType e .
      applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
    applyToIntegrationQuestion e =
      applyProps e .
      applyIntegrationUuid e .
      applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
    applyTitle e q = applyValue (e ^. title) q qChangeTitle
    applyText e q = applyValue (e ^. text) q qChangeText
    applyRequiredLevel e q = applyValue (e ^. requiredLevel) q qChangeRequiredLevel
    applyTagUuids e q = applyValue (e ^. tagUuids) q qChangeTagUuids
    applyExpertUuids e q = applyValue (e ^. expertUuids) q qChangeExpertUuidsOrder
    applyReferenceUuids e q = applyValue (e ^. referenceUuids) q qChangeReferenceUuidsOrder
    applyAnwerUuids e q = applyValue (e ^. answerUuids) q qChangeAnwerUuidsOrder
    applyItemTemplateTitle e q = applyValue (e ^. itemTemplateTitle) q qChangeItemTemplateTitle
    applyItemTemplateQuestions e q = applyValue (e ^. itemTemplateQuestionUuids) q qChangeItemTemplateQuestionUuidsOrder
    applyValueType e q = applyValue (e ^. valueType) q qChangeValueType
    applyIntegrationUuid e q = applyValue (e ^. integrationUuid) q qChangeIntegrationUuid
    applyProps e q = applyValue (e ^. props) q qChangeProps

convertToOptionsQuestion :: Question -> Question
convertToOptionsQuestion (OptionsQuestion' q) = OptionsQuestion' q
convertToOptionsQuestion q' =
  case q' of
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      OptionsQuestion' $
      OptionsQuestion
      { _optionsQuestionUuid = q ^. uuid
      , _optionsQuestionTitle = q ^. title
      , _optionsQuestionText = q ^. text
      , _optionsQuestionRequiredLevel = q ^. requiredLevel
      , _optionsQuestionTagUuids = q ^. tagUuids
      , _optionsQuestionReferences = q ^. references
      , _optionsQuestionExperts = q ^. experts
      , _optionsQuestionAnswers = []
      }

convertToListQuestion :: Question -> Question
convertToListQuestion (ListQuestion' q) = ListQuestion' q
convertToListQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      ListQuestion' $
      ListQuestion
      { _listQuestionUuid = q ^. uuid
      , _listQuestionTitle = q ^. title
      , _listQuestionText = q ^. text
      , _listQuestionRequiredLevel = q ^. requiredLevel
      , _listQuestionTagUuids = q ^. tagUuids
      , _listQuestionReferences = q ^. references
      , _listQuestionExperts = q ^. experts
      , _listQuestionItemTemplateTitle = ""
      , _listQuestionItemTemplateQuestions = []
      }

convertToValueQuestion :: Question -> Question
convertToValueQuestion (ValueQuestion' q) = ValueQuestion' q
convertToValueQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      ValueQuestion' $
      ValueQuestion
      { _valueQuestionUuid = q ^. uuid
      , _valueQuestionTitle = q ^. title
      , _valueQuestionText = q ^. text
      , _valueQuestionRequiredLevel = q ^. requiredLevel
      , _valueQuestionTagUuids = q ^. tagUuids
      , _valueQuestionReferences = q ^. references
      , _valueQuestionExperts = q ^. experts
      , _valueQuestionValueType = StringQuestionValueType
      }

convertToIntegrationQuestion :: Question -> Question
convertToIntegrationQuestion (IntegrationQuestion' q) = IntegrationQuestion' q
convertToIntegrationQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
  where
    createQuestion q =
      IntegrationQuestion' $
      IntegrationQuestion
      { _integrationQuestionUuid = q ^. uuid
      , _integrationQuestionTitle = q ^. title
      , _integrationQuestionText = q ^. text
      , _integrationQuestionRequiredLevel = q ^. requiredLevel
      , _integrationQuestionTagUuids = q ^. tagUuids
      , _integrationQuestionReferences = q ^. references
      , _integrationQuestionExperts = q ^. experts
      , _integrationQuestionIntegrationUuid = U.nil
      , _integrationQuestionProps = empty
      }

updateIntegrationProps :: EditIntegrationEvent -> Question -> Question
updateIntegrationProps e (IntegrationQuestion' q) = IntegrationQuestion' $ q & props .~ updatedProps
  where
    updatedProps =
      if q ^. integrationUuid == e ^. integrationUuid
        then case e ^. props of
               ChangedValue ps -> fromList . fmap (\p -> (p, fromMaybe "" (lookup p (q ^. props)))) $ ps
               NothingChanged -> q ^. props
        else q ^. props
updateIntegrationProps _ q' = q'

-- -------------------
addItemTemplateQuestion :: Question -> Question -> Question
addItemTemplateQuestion q itQ = q & qChangeItemTemplateQuestions .~ modifiedItemTemplateQuestions
  where
    modifiedItemTemplateQuestions = (getItemTemplateQuestions q) ++ [itQ]

deleteItemTemplateQuestion :: Question -> U.UUID -> Question
deleteItemTemplateQuestion q itQUuid = q & qChangeItemTemplateQuestions .~ modifiedItemTemplateQuestions
  where
    modifiedItemTemplateQuestions = filter (\q -> getQuestionUuid q /= itQUuid) (getItemTemplateQuestions q)

addAnswer :: Question -> Answer -> Question
addAnswer q ans = q & qChangeAnswers .~ modifiedAnswers
  where
    modifiedAnswers = getAnswers q ++ [ans]

deleteAnswer :: Question -> U.UUID -> Question
deleteAnswer q ansUuid = q & qChangeAnswers .~ modifiedAnswers
  where
    modifiedAnswers = filter (\ans -> ans ^. uuid /= ansUuid) (getAnswers q)

addExpert :: Question -> Expert -> Question
addExpert q exp = q & qChangeExperts .~ (getExperts q ++ [exp])

deleteExpert :: Question -> U.UUID -> Question
deleteExpert q expUuid = q & qChangeExperts .~ (filter (\exp -> exp ^. uuid /= expUuid) (getExperts q))

addReference :: Question -> Reference -> Question
addReference q ref = q & qChangeReferences .~ (getReferences q ++ [ref])

deleteReference :: Question -> U.UUID -> Question
deleteReference q refUuid =
  q & qChangeReferences .~ (filter (\ref -> (getReferenceUuid ref) /= refUuid) (getReferences q))

-- -------------------
-- ANSWER ------------
-- -------------------
createAnswer :: AddAnswerEvent -> Answer
createAnswer e =
  Answer
  { _answerUuid = e ^. answerUuid
  , _answerLabel = e ^. label
  , _answerAdvice = e ^. advice
  , _answerFollowUps = []
  , _answerMetricMeasures = e ^. metricMeasures
  }

editAnswer :: EditAnswerEvent -> Answer -> Answer
editAnswer e = applyMetricMeasures . applyFollowUps . applyAdvice . applyLabel
  where
    applyLabel ans = applyValue (e ^. label) ans label
    applyAdvice ans = applyValue (e ^. advice) ans advice
    applyFollowUps ans = applyValue (e ^. followUpUuids) ans ansChangeFollowUpUuidsOrder
    applyMetricMeasures ans = applyValue (e ^. metricMeasures) ans metricMeasures

-- -------------------
addFuQuestion :: Answer -> Question -> Answer
addFuQuestion ans q = ans & followUps .~ (ans ^. followUps ++ [q])

deleteFuQuestion :: Answer -> U.UUID -> Answer
deleteFuQuestion ans qUuid = ans & followUps .~ (filter (\q -> getQuestionUuid q /= qUuid) (ans ^. followUps))

-- -------------------
-- EXPERT ------------
-- -------------------
createExpert :: AddExpertEvent -> Expert
createExpert e = Expert {_expertUuid = e ^. expertUuid, _expertName = e ^. name, _expertEmail = e ^. email}

editExpert :: EditExpertEvent -> Expert -> Expert
editExpert e = applyEmail . applyName
  where
    applyName exp = applyValue (e ^. name) exp name
    applyEmail exp = applyValue (e ^. email) exp email

-- -------------------
-- REFERENCE ---------
-- -------------------
createReference :: AddReferenceEvent -> Reference
createReference (AddResourcePageReferenceEvent' e) =
  ResourcePageReference' $
  ResourcePageReference
  {_resourcePageReferenceUuid = e ^. referenceUuid, _resourcePageReferenceShortUuid = e ^. shortUuid}
createReference (AddURLReferenceEvent' e) =
  URLReference' $
  URLReference {_uRLReferenceUuid = e ^. referenceUuid, _uRLReferenceUrl = e ^. url, _uRLReferenceLabel = e ^. label}
createReference (AddCrossReferenceEvent' e) =
  CrossReference' $
  CrossReference
  { _crossReferenceUuid = e ^. referenceUuid
  , _crossReferenceTargetUuid = e ^. targetUuid
  , _crossReferenceDescription = e ^. description
  }

editReference e' ref =
  case e' of
    (EditResourcePageReferenceEvent' e) ->
      ResourcePageReference' . applyToResourcePageReference e . convertToResourcePageReference $ ref
    (EditURLReferenceEvent' e) -> URLReference' . applyToURLReference e . convertToURLReference $ ref
    (EditCrossReferenceEvent' e) -> CrossReference' . applyToCrossReference e . convertToCrossReference $ ref
  where
    applyToResourcePageReference e = applyShortUuid e
    applyToURLReference e = applyAnchor e . applyUrl e
    applyToCrossReference e = applyDescription e . applyTarget e
    applyShortUuid e ref = applyValue (e ^. shortUuid) ref shortUuid
    applyUrl e ref = applyValue (e ^. url) ref url
    applyAnchor e ref = applyValue (e ^. label) ref label
    applyTarget e ref = applyValue (e ^. targetUuid) ref targetUuid
    applyDescription e ref = applyValue (e ^. description) ref description

convertToResourcePageReference :: Reference -> ResourcePageReference
convertToResourcePageReference (ResourcePageReference' ref) = ref
convertToResourcePageReference ref' =
  case ref' of
    (URLReference' ref) -> createQuestion ref
    (CrossReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      ResourcePageReference {_resourcePageReferenceUuid = ref ^. uuid, _resourcePageReferenceShortUuid = ""}

convertToURLReference :: Reference -> URLReference
convertToURLReference (URLReference' ref) = ref
convertToURLReference ref' =
  case ref' of
    (ResourcePageReference' ref) -> createQuestion ref
    (CrossReference' ref) -> createQuestion ref
  where
    createQuestion ref = URLReference {_uRLReferenceUuid = ref ^. uuid, _uRLReferenceUrl = "", _uRLReferenceLabel = ""}

convertToCrossReference :: Reference -> CrossReference
convertToCrossReference (CrossReference' ref) = ref
convertToCrossReference ref' =
  case ref' of
    (ResourcePageReference' ref) -> createQuestion ref
    (URLReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      CrossReference
      {_crossReferenceUuid = ref ^. uuid, _crossReferenceTargetUuid = U.nil, _crossReferenceDescription = ""}

-- -------------------
-- TAG ---------------
-- -------------------
createTag :: AddTagEvent -> Tag
createTag e =
  Tag {_tagUuid = e ^. tagUuid, _tagName = e ^. name, _tagDescription = e ^. description, _tagColor = e ^. color}

editTag :: EditTagEvent -> Tag -> Tag
editTag e = applyColor . applyDescription . applyName
  where
    applyName tag = applyValue (e ^. name) tag name
    applyDescription tag = applyValue (e ^. description) tag description
    applyColor tag = applyValue (e ^. color) tag color

-- -------------------
-- INTEGRATION -------
-- -------------------
createIntegration :: AddIntegrationEvent -> Integration
createIntegration e =
  Integration
  { _integrationUuid = e ^. integrationUuid
  , _integrationIId = e ^. iId
  , _integrationName = e ^. name
  , _integrationProps = e ^. props
  , _integrationLogo = e ^. logo
  , _integrationRequestMethod = e ^. requestMethod
  , _integrationRequestUrl = e ^. requestUrl
  , _integrationRequestHeaders = e ^. requestHeaders
  , _integrationRequestBody = e ^. requestBody
  , _integrationResponseListField = e ^. responseListField
  , _integrationResponseIdField = e ^. responseIdField
  , _integrationResponseNameField = e ^. responseNameField
  , _integrationItemUrl = e ^. itemUrl
  }

editIntegration :: EditIntegrationEvent -> Integration -> Integration
editIntegration e =
  applyIId .
  applyName .
  applyProps .
  applyLogo .
  applyRequestMethod .
  applyRequestUrl .
  applyRequestHeaders .
  applyRequestBody . applyResponseListField . applyResponseIdField . applyResponseNameField . applyItemUrl
  where
    applyIId integration = applyValue (e ^. iId) integration iId
    applyName integration = applyValue (e ^. name) integration name
    applyProps integration = applyValue (e ^. props) integration props
    applyLogo integration = applyValue (e ^. logo) integration logo
    applyRequestMethod integration = applyValue (e ^. requestMethod) integration requestMethod
    applyRequestUrl integration = applyValue (e ^. requestUrl) integration requestUrl
    applyRequestHeaders integration = applyValue (e ^. requestHeaders) integration requestHeaders
    applyRequestBody integration = applyValue (e ^. requestBody) integration requestBody
    applyResponseListField integration = applyValue (e ^. responseListField) integration responseListField
    applyResponseIdField integration = applyValue (e ^. responseIdField) integration responseIdField
    applyResponseNameField integration = applyValue (e ^. responseNameField) integration responseNameField
    applyItemUrl integration = applyValue (e ^. itemUrl) integration itemUrl
