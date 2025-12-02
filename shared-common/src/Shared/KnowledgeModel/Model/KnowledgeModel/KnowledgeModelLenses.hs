module Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses (
  module Shared.Common.Model.Common.Lens,
  module Shared.KnowledgeModel.Model.Common.Lens,
  toMap,
) where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
instance HasKnowledgeModelChapters KnowledgeModel where
  getChaptersL km = M.elems km.entities.chapters
  setChaptersL km newChapters = km {entities = km.entities {chapters = toMap newChapters}}
  getChaptersM km = km.entities.chapters
  setChaptersM km newChapters = km {entities = km.entities {chapters = newChapters}}
  putInChaptersM chapterUuid chapter km = setChaptersM km $ M.insert chapterUuid chapter km.entities.chapters
  deleteFromChaptersM chapterUuid km = setChaptersM km $ M.delete chapterUuid km.entities.chapters

------------------------------------------------------------------------------------------
instance HasKnowledgeModelQuestions KnowledgeModel where
  getQuestionsL km = M.elems km.entities.questions
  setQuestionsL km newQuestions = km {entities = km.entities {questions = toMap newQuestions}}
  getQuestionsM km = km.entities.questions
  setQuestionsM km newQuestions = km {entities = km.entities {questions = newQuestions}}
  putInQuestionsM questionUuid question km = setQuestionsM km $ M.insert questionUuid question km.entities.questions
  deleteFromQuestionsM questionUuid km = setQuestionsM km $ M.delete questionUuid km.entities.questions

------------------------------------------------------------------------------------------
instance HasKnowledgeModelAnswers KnowledgeModel where
  getAnswersL km = M.elems km.entities.answers
  setAnswersL km newAnswers = km {entities = km.entities {answers = toMap newAnswers}}
  getAnswersM km = km.entities.answers
  setAnswersM km newAnswers = km {entities = km.entities {answers = newAnswers}}
  putInAnswersM answerUuid answer km = setAnswersM km $ M.insert answerUuid answer km.entities.answers
  deleteFromAnswersM answerUuid km = setAnswersM km $ M.delete answerUuid km.entities.answers

------------------------------------------------------------------------------------------
instance HasKnowledgeModelChoices KnowledgeModel where
  getChoicesL km = M.elems km.entities.choices
  setChoicesL km newChoices = km {entities = km.entities {choices = toMap newChoices}}
  getChoicesM km = km.entities.choices
  setChoicesM km newChoices = km {entities = km.entities {choices = newChoices}}
  putInChoicesM choiceUuid choice km = setChoicesM km $ M.insert choiceUuid choice km.entities.choices
  deleteFromChoicesM choiceUuid km = setChoicesM km $ M.delete choiceUuid km.entities.choices

------------------------------------------------------------------------------------------
instance HasKnowledgeModelExperts KnowledgeModel where
  getExpertsL km = M.elems km.entities.experts
  setExpertsL km newExperts = km {entities = km.entities {experts = toMap newExperts}}
  getExpertsM km = km.entities.experts
  setExpertsM km newExperts = km {entities = km.entities {experts = newExperts}}
  putInExpertsM expertUuid expert km = setExpertsM km $ M.insert expertUuid expert km.entities.experts
  deleteFromExpertsM expertUuid km = setExpertsM km $ M.delete expertUuid km.entities.experts

------------------------------------------------------------------------------------------
instance HasKnowledgeModelReferences KnowledgeModel where
  getReferencesL km = M.elems km.entities.references
  setReferencesL km newReferences = km {entities = km.entities {references = toMap newReferences}}
  getReferencesM km = km.entities.references
  setReferencesM km newReferences = km {entities = km.entities {references = newReferences}}
  putInReferencesM referenceUuid reference km = setReferencesM km $ M.insert referenceUuid reference km.entities.references
  deleteFromReferencesM referenceUuid km = setReferencesM km $ M.delete referenceUuid km.entities.references

------------------------------------------------------------------------------------------
instance HasKnowledgeModelIntegrations KnowledgeModel where
  getIntegrationsL km = M.elems km.entities.integrations
  setIntegrationsL km newIntegrations = km {entities = km.entities {integrations = toMap newIntegrations}}
  getIntegrationsM km = km.entities.integrations
  setIntegrationsM km newIntegrations = km {entities = km.entities {integrations = newIntegrations}}
  putInIntegrationsM integrationUuid integration km = setIntegrationsM km $ M.insert integrationUuid integration km.entities.integrations
  deleteFromIntegrationsM integrationUuid km = setIntegrationsM km $ M.delete integrationUuid km.entities.integrations

------------------------------------------------------------------------------------------
instance HasKnowledgeModelTags KnowledgeModel where
  getTagsL km = M.elems km.entities.tags
  setTagsL km newTags = km {entities = km.entities {tags = toMap newTags}}
  getTagsM km = km.entities.tags
  setTagsM km newTags = km {entities = km.entities {tags = newTags}}
  putInTagsM tagUuid tag km = setTagsM km $ M.insert tagUuid tag km.entities.tags
  deleteFromTagsM tagUuid km = setTagsM km $ M.delete tagUuid km.entities.tags

------------------------------------------------------------------------------------------
instance HasKnowledgeModelMetrics KnowledgeModel where
  getMetricsL km = M.elems km.entities.metrics
  setMetricsL km newMetrics = km {entities = km.entities {metrics = toMap newMetrics}}
  getMetricsM km = km.entities.metrics
  setMetricsM km newMetrics = km {entities = km.entities {metrics = newMetrics}}
  putInMetricsM metricUuid metric km = setMetricsM km $ M.insert metricUuid metric km.entities.metrics
  deleteFromMetricsM metricUuid km = setMetricsM km $ M.delete metricUuid km.entities.metrics

------------------------------------------------------------------------------------------
instance HasKnowledgeModelPhases KnowledgeModel where
  getPhasesL km = M.elems km.entities.phases
  setPhasesL km newPhases = km {entities = km.entities {phases = toMap newPhases}}
  getPhasesM km = km.entities.phases
  setPhasesM km newPhases = km {entities = km.entities {phases = newPhases}}
  putInPhasesM phaseUuid phase km = setPhasesM km $ M.insert phaseUuid phase km.entities.phases
  deleteFromPhasesM phaseUuid km = setPhasesM km $ M.delete phaseUuid km.entities.phases

------------------------------------------------------------------------------------------
instance HasKnowledgeModelResourceCollection KnowledgeModel where
  getResourceCollectionsL km = M.elems km.entities.resourceCollections
  setResourceCollectionsL km newResourceCollections = km {entities = km.entities {resourceCollections = toMap newResourceCollections}}
  getResourceCollectionsM km = km.entities.resourceCollections
  setResourceCollectionsM km newResourceCollections = km {entities = km.entities {resourceCollections = newResourceCollections}}
  putInResourceCollectionsM resourceCollectionUuid phase km = setResourceCollectionsM km $ M.insert resourceCollectionUuid phase km.entities.resourceCollections
  deleteFromResourceCollectionsM resourceCollectionUuid km = setResourceCollectionsM km $ M.delete resourceCollectionUuid km.entities.resourceCollections

------------------------------------------------------------------------------------------
instance HasKnowledgeModelResourcePage KnowledgeModel where
  getResourcePagesL km = M.elems km.entities.resourcePages
  setResourcePagesL km newResourcePages = km {entities = km.entities {resourcePages = toMap newResourcePages}}
  getResourcePagesM km = km.entities.resourcePages
  setResourcePagesM km newResourcePages = km {entities = km.entities {resourcePages = newResourcePages}}
  putInResourcePagesM resourcePageUuid phase km = setResourcePagesM km $ M.insert resourcePageUuid phase km.entities.resourcePages
  deleteFromResourcePagesM resourcePageUuid km = setResourcePagesM km $ M.delete resourcePageUuid km.entities.resourcePages

------------------------------------------------------------------------------------------
toMap :: HasUuid' a => [a] -> M.Map U.UUID a
toMap = M.fromList . fmap (\entity -> (getUuid entity, entity))

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasUuid' KnowledgeModel where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' Chapter where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' Question where
  getUuid (OptionsQuestion' entity) = entity.uuid
  getUuid (MultiChoiceQuestion' entity) = entity.uuid
  getUuid (ListQuestion' entity) = entity.uuid
  getUuid (ValueQuestion' entity) = entity.uuid
  getUuid (IntegrationQuestion' entity) = entity.uuid
  getUuid (ItemSelectQuestion' entity) = entity.uuid
  getUuid (FileQuestion' entity) = entity.uuid
  setUuid (OptionsQuestion' entity) newValue = OptionsQuestion' $ entity {uuid = newValue}
  setUuid (MultiChoiceQuestion' entity) newValue = MultiChoiceQuestion' $ entity {uuid = newValue}
  setUuid (ListQuestion' entity) newValue = ListQuestion' $ entity {uuid = newValue}
  setUuid (ValueQuestion' entity) newValue = ValueQuestion' $ entity {uuid = newValue}
  setUuid (IntegrationQuestion' entity) newValue = IntegrationQuestion' $ entity {uuid = newValue}
  setUuid (ItemSelectQuestion' entity) newValue = ItemSelectQuestion' $ entity {uuid = newValue}
  setUuid (FileQuestion' entity) newValue = FileQuestion' $ entity {uuid = newValue}

instance HasUuid' Expert where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' Reference where
  getUuid (ResourcePageReference' entity) = entity.uuid
  getUuid (URLReference' entity) = entity.uuid
  getUuid (CrossReference' entity) = entity.uuid
  setUuid (ResourcePageReference' entity) newValue = ResourcePageReference' $ entity {uuid = newValue}
  setUuid (URLReference' entity) newValue = URLReference' $ entity {uuid = newValue}
  setUuid (CrossReference' entity) newValue = CrossReference' $ entity {uuid = newValue}

instance HasUuid' Answer where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' Choice where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' Tag where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' Integration where
  getUuid (ApiIntegration' entity) = entity.uuid
  getUuid (ApiLegacyIntegration' entity) = entity.uuid
  getUuid (WidgetIntegration' entity) = entity.uuid
  setUuid (ApiIntegration' entity) newValue = ApiIntegration' $ entity {uuid = newValue}
  setUuid (ApiLegacyIntegration' entity) newValue = ApiLegacyIntegration' $ entity {uuid = newValue}
  setUuid (WidgetIntegration' entity) newValue = WidgetIntegration' $ entity {uuid = newValue}

instance HasUuid' Metric where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' Phase where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' ResourceCollection where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' ResourcePage where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
instance HasAnnotations' KnowledgeModel where
  getAnnotations entity = entity.annotations
  setAnnotations entity newValue = entity {annotations = newValue}

instance HasAnnotations' Chapter where
  getAnnotations entity = entity.annotations
  setAnnotations entity newValue = entity {annotations = newValue}

instance HasAnnotations' Question where
  getAnnotations (OptionsQuestion' entity) = entity.annotations
  getAnnotations (MultiChoiceQuestion' entity) = entity.annotations
  getAnnotations (ListQuestion' entity) = entity.annotations
  getAnnotations (ValueQuestion' entity) = entity.annotations
  getAnnotations (IntegrationQuestion' entity) = entity.annotations
  getAnnotations (ItemSelectQuestion' entity) = entity.annotations
  getAnnotations (FileQuestion' entity) = entity.annotations
  setAnnotations (OptionsQuestion' entity) newValue = OptionsQuestion' $ entity {annotations = newValue}
  setAnnotations (MultiChoiceQuestion' entity) newValue = MultiChoiceQuestion' $ entity {annotations = newValue}
  setAnnotations (ListQuestion' entity) newValue = ListQuestion' $ entity {annotations = newValue}
  setAnnotations (ValueQuestion' entity) newValue = ValueQuestion' $ entity {annotations = newValue}
  setAnnotations (IntegrationQuestion' entity) newValue = IntegrationQuestion' $ entity {annotations = newValue}
  setAnnotations (ItemSelectQuestion' entity) newValue = ItemSelectQuestion' $ entity {annotations = newValue}
  setAnnotations (FileQuestion' entity) newValue = FileQuestion' $ entity {annotations = newValue}

instance HasAnnotations' Expert where
  getAnnotations entity = entity.annotations
  setAnnotations entity newValue = entity {annotations = newValue}

instance HasAnnotations' Reference where
  getAnnotations (ResourcePageReference' entity) = entity.annotations
  getAnnotations (URLReference' entity) = entity.annotations
  getAnnotations (CrossReference' entity) = entity.annotations
  setAnnotations (ResourcePageReference' entity) newValue = ResourcePageReference' $ entity {annotations = newValue}
  setAnnotations (URLReference' entity) newValue = URLReference' $ entity {annotations = newValue}
  setAnnotations (CrossReference' entity) newValue = CrossReference' $ entity {annotations = newValue}

instance HasAnnotations' Answer where
  getAnnotations entity = entity.annotations
  setAnnotations entity newValue = entity {annotations = newValue}

instance HasAnnotations' Choice where
  getAnnotations entity = entity.annotations
  setAnnotations entity newValue = entity {annotations = newValue}

instance HasAnnotations' Tag where
  getAnnotations entity = entity.annotations
  setAnnotations entity newValue = entity {annotations = newValue}

instance HasAnnotations' Integration where
  getAnnotations (ApiIntegration' entity) = entity.annotations
  getAnnotations (ApiLegacyIntegration' entity) = entity.annotations
  getAnnotations (WidgetIntegration' entity) = entity.annotations
  setAnnotations (ApiIntegration' entity) newValue = ApiIntegration' $ entity {annotations = newValue}
  setAnnotations (ApiLegacyIntegration' entity) newValue = ApiLegacyIntegration' $ entity {annotations = newValue}
  setAnnotations (WidgetIntegration' entity) newValue = WidgetIntegration' $ entity {annotations = newValue}

instance HasAnnotations' Metric where
  getAnnotations entity = entity.annotations
  setAnnotations entity newValue = entity {annotations = newValue}

instance HasAnnotations' Phase where
  getAnnotations entity = entity.annotations
  setAnnotations entity newValue = entity {annotations = newValue}

------------------------------------------------------------------------------------------
instance HasTitle' Question where
  getTitle (OptionsQuestion' q) = q.title
  getTitle (MultiChoiceQuestion' q) = q.title
  getTitle (ListQuestion' q) = q.title
  getTitle (ValueQuestion' q) = q.title
  getTitle (IntegrationQuestion' q) = q.title
  getTitle (ItemSelectQuestion' q) = q.title
  getTitle (FileQuestion' q) = q.title
  setTitle (OptionsQuestion' q) newValue = OptionsQuestion' $ q {title = newValue}
  setTitle (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {title = newValue}
  setTitle (ListQuestion' q) newValue = ListQuestion' $ q {title = newValue}
  setTitle (ValueQuestion' q) newValue = ValueQuestion' $ q {title = newValue}
  setTitle (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {title = newValue}
  setTitle (ItemSelectQuestion' q) newValue = ItemSelectQuestion' $ q {title = newValue}
  setTitle (FileQuestion' q) newValue = FileQuestion' $ q {title = newValue}

------------------------------------------------------------------------------------------
instance HasText' Question where
  getText (OptionsQuestion' q) = q.text
  getText (MultiChoiceQuestion' q) = q.text
  getText (ListQuestion' q) = q.text
  getText (ValueQuestion' q) = q.text
  getText (IntegrationQuestion' q) = q.text
  getText (ItemSelectQuestion' q) = q.text
  getText (FileQuestion' q) = q.text
  setText (OptionsQuestion' q) newValue = OptionsQuestion' $ q {text = newValue}
  setText (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {text = newValue}
  setText (ListQuestion' q) newValue = ListQuestion' $ q {text = newValue}
  setText (ValueQuestion' q) newValue = ValueQuestion' $ q {text = newValue}
  setText (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {text = newValue}
  setText (ItemSelectQuestion' q) newValue = ItemSelectQuestion' $ q {text = newValue}
  setText (FileQuestion' q) newValue = FileQuestion' $ q {text = newValue}

------------------------------------------------------------------------------------------
instance HasRequiredPhaseUuid' Question where
  getRequiredPhaseUuid (OptionsQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (MultiChoiceQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (ListQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (ValueQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (IntegrationQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (ItemSelectQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (FileQuestion' q) = q.requiredPhaseUuid
  setRequiredPhaseUuid (OptionsQuestion' q) newValue = OptionsQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (ListQuestion' q) newValue = ListQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (ValueQuestion' q) newValue = ValueQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (ItemSelectQuestion' q) newValue = ItemSelectQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (FileQuestion' q) newValue = FileQuestion' $ q {requiredPhaseUuid = newValue}

------------------------------------------------------------------------------------------
instance HasTagUuids' Question where
  getTagUuids (OptionsQuestion' q) = q.tagUuids
  getTagUuids (MultiChoiceQuestion' q) = q.tagUuids
  getTagUuids (ListQuestion' q) = q.tagUuids
  getTagUuids (ValueQuestion' q) = q.tagUuids
  getTagUuids (IntegrationQuestion' q) = q.tagUuids
  getTagUuids (ItemSelectQuestion' q) = q.tagUuids
  getTagUuids (FileQuestion' q) = q.tagUuids
  setTagUuids (OptionsQuestion' q) newValue = OptionsQuestion' $ q {tagUuids = newValue}
  setTagUuids (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {tagUuids = newValue}
  setTagUuids (ListQuestion' q) newValue = ListQuestion' $ q {tagUuids = newValue}
  setTagUuids (ValueQuestion' q) newValue = ValueQuestion' $ q {tagUuids = newValue}
  setTagUuids (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {tagUuids = newValue}
  setTagUuids (ItemSelectQuestion' q) newValue = ItemSelectQuestion' $ q {tagUuids = newValue}
  setTagUuids (FileQuestion' q) newValue = FileQuestion' $ q {tagUuids = newValue}

-- ------------------------------------------------------------------------------------------
instance HasExpertUuids' Question [U.UUID] where
  getExpertUuids (OptionsQuestion' entity) = entity.expertUuids
  getExpertUuids (MultiChoiceQuestion' entity) = entity.expertUuids
  getExpertUuids (ListQuestion' entity) = entity.expertUuids
  getExpertUuids (ValueQuestion' entity) = entity.expertUuids
  getExpertUuids (IntegrationQuestion' entity) = entity.expertUuids
  getExpertUuids (ItemSelectQuestion' entity) = entity.expertUuids
  getExpertUuids (FileQuestion' entity) = entity.expertUuids
  setExpertUuids (OptionsQuestion' entity) newValue = OptionsQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (MultiChoiceQuestion' entity) newValue = MultiChoiceQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (ListQuestion' entity) newValue = ListQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (ValueQuestion' entity) newValue = ValueQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (IntegrationQuestion' entity) newValue = IntegrationQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (ItemSelectQuestion' entity) newValue = ItemSelectQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (FileQuestion' entity) newValue = FileQuestion' $ entity {expertUuids = newValue}

-- ------------------------------------------------------------------------------------------
instance HasReferenceUuids' Question [U.UUID] where
  getReferenceUuids (OptionsQuestion' q) = q.referenceUuids
  getReferenceUuids (MultiChoiceQuestion' q) = q.referenceUuids
  getReferenceUuids (ListQuestion' q) = q.referenceUuids
  getReferenceUuids (ValueQuestion' q) = q.referenceUuids
  getReferenceUuids (IntegrationQuestion' q) = q.referenceUuids
  getReferenceUuids (ItemSelectQuestion' q) = q.referenceUuids
  getReferenceUuids (FileQuestion' q) = q.referenceUuids
  setReferenceUuids (OptionsQuestion' q) newValue = OptionsQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (ListQuestion' q) newValue = ListQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (ValueQuestion' q) newValue = ValueQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (ItemSelectQuestion' q) newValue = ItemSelectQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (FileQuestion' q) newValue = FileQuestion' $ q {referenceUuids = newValue}

-- ------------------------------------------------------------------------------------------
instance HasAnswerUuids' Question where
  getAnswerUuids (OptionsQuestion' q) = q.answerUuids
  getAnswerUuids q = []
  setAnswerUuids (OptionsQuestion' q) newValue = OptionsQuestion' $ q {answerUuids = newValue}
  setAnswerUuids q newValue = q

-- ------------------------------------------------------------------------------------------
instance HasChoiceUuids' Question where
  getChoiceUuids (MultiChoiceQuestion' q) = q.choiceUuids
  getChoiceUuids q = []
  setChoiceUuids (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {choiceUuids = newValue}
  setChoiceUuids q newValue = q

-- ------------------------------------------------------------------------------------------
instance HasItemTemplateQuestionUuids' Question where
  getItemTemplateQuestionUuids (ListQuestion' q) = q.itemTemplateQuestionUuids
  getItemTemplateQuestionUuids q = []
  setItemTemplateQuestionUuids (ListQuestion' q) newValue = ListQuestion' $ q {itemTemplateQuestionUuids = newValue}
  setItemTemplateQuestionUuids q newValue = q

-- ------------------------------------------------------------------------------------------
instance HasValueType' Question where
  getValueType (ValueQuestion' q) = q.valueType
  getValueType q = StringQuestionValueType
  setValueType (ValueQuestion' q) newValue = ValueQuestion' $ q {valueType = newValue}
  setValueType q newValue = q

-- ------------------------------------------------------------------------------------------
instance HasIntegrationUuid' Question where
  getIntegrationUuid (IntegrationQuestion' q) = q.integrationUuid
  getIntegrationUuid q = U.nil
  setIntegrationUuid (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {integrationUuid = newValue}
  setIntegrationUuid q newValue = q

-- ------------------------------------------------------------------------------------------
instance HasVariables' Question (M.Map String String) where
  getVariables (IntegrationQuestion' q) = q.variables
  getVariables q = M.empty
  setVariables (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {variables = newValue}
  setVariables q newValue = q

instance HasVariables' Integration [String] where
  getVariables (ApiIntegration' integration) = integration.variables
  getVariables (ApiLegacyIntegration' integration) = integration.variables
  getVariables (WidgetIntegration' integration) = integration.variables
  setVariables (ApiIntegration' integration) newValue = ApiIntegration' $ integration {variables = newValue}
  setVariables (ApiLegacyIntegration' integration) newValue = ApiLegacyIntegration' $ integration {variables = newValue}
  setVariables (WidgetIntegration' integration) newValue = WidgetIntegration' $ integration {variables = newValue}
