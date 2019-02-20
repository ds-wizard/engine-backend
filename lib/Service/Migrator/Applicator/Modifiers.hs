module Service.Migrator.Applicator.Modifiers where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U

import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors

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
  }

editKM :: EditKnowledgeModelEvent -> KnowledgeModel -> KnowledgeModel
editKM e = applyTagUuids . applyChapterUuids . applyName
  where
    applyName km = applyValue (e ^. name) km name
    applyChapterUuids km = applyValue (e ^. chapterUuids) km kmChangeChapterUuidsOrder
    applyTagUuids km = applyValue (e ^. tagUuids) km kmChangeTagUuidsOrder

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

editQuestion :: EditQuestionEvent -> Question -> Question
editQuestion e' q =
  case e' of
    (EditOptionsQuestionEvent' e) ->
      applyAnwerUuids e .
      applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e $
      q
    (EditListQuestionEvent' e) ->
      applyItemTemplateQuestions e .
      applyItemTemplateTitle e .
      applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e $
      q
    (EditValueQuestionEvent' e) ->
      applyValueType e .
      applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e $
      q
  where
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

editReference :: EditReferenceEvent -> Reference -> Reference
editReference (EditResourcePageReferenceEvent' e) (ResourcePageReference' ref) =
  ResourcePageReference' . applyShortUuid $ ref
  where
    applyShortUuid ref = applyValue (e ^. shortUuid) ref shortUuid
editReference (EditURLReferenceEvent' e) (URLReference' ref) = URLReference' . applyAnchor . applyUrl $ ref
  where
    applyUrl ref = applyValue (e ^. url) ref url
    applyAnchor ref = applyValue (e ^. label) ref label
editReference (EditCrossReferenceEvent' e) (CrossReference' ref) =
  CrossReference' . applyDescription . applyTarget $ ref
  where
    applyTarget ref = applyValue (e ^. targetUuid) ref targetUuid
    applyDescription ref = applyValue (e ^. description) ref description

-- -------------------
-- TAG ------------
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
