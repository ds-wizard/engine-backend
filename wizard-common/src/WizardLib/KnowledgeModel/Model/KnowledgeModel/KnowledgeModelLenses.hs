module WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses (
  module Shared.Common.Model.Common.Lens,
  module WizardLib.KnowledgeModel.Model.Common.Lens,
  toMap,
) where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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
  putInReferencesM referenceUuid refenrece km = setReferencesM km $ M.insert referenceUuid refenrece km.entities.references
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
  setUuid (OptionsQuestion' entity) newValue = OptionsQuestion' $ entity {uuid = newValue}
  setUuid (MultiChoiceQuestion' entity) newValue = MultiChoiceQuestion' $ entity {uuid = newValue}
  setUuid (ListQuestion' entity) newValue = ListQuestion' $ entity {uuid = newValue}
  setUuid (ValueQuestion' entity) newValue = ValueQuestion' $ entity {uuid = newValue}
  setUuid (IntegrationQuestion' entity) newValue = IntegrationQuestion' $ entity {uuid = newValue}

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
  getUuid (WidgetIntegration' entity) = entity.uuid
  setUuid (ApiIntegration' entity) newValue = ApiIntegration' $ entity {uuid = newValue}
  setUuid (WidgetIntegration' entity) newValue = WidgetIntegration' $ entity {uuid = newValue}

instance HasUuid' Metric where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' Phase where
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
  setAnnotations (OptionsQuestion' entity) newValue = OptionsQuestion' $ entity {annotations = newValue}
  setAnnotations (MultiChoiceQuestion' entity) newValue = MultiChoiceQuestion' $ entity {annotations = newValue}
  setAnnotations (ListQuestion' entity) newValue = ListQuestion' $ entity {annotations = newValue}
  setAnnotations (ValueQuestion' entity) newValue = ValueQuestion' $ entity {annotations = newValue}
  setAnnotations (IntegrationQuestion' entity) newValue = IntegrationQuestion' $ entity {annotations = newValue}

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
  getAnnotations (WidgetIntegration' entity) = entity.annotations
  setAnnotations (ApiIntegration' entity) newValue = ApiIntegration' $ entity {annotations = newValue}
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
  setTitle (OptionsQuestion' q) newValue = OptionsQuestion' $ q {title = newValue}
  setTitle (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {title = newValue}
  setTitle (ListQuestion' q) newValue = ListQuestion' $ q {title = newValue}
  setTitle (ValueQuestion' q) newValue = ValueQuestion' $ q {title = newValue}
  setTitle (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {title = newValue}

------------------------------------------------------------------------------------------
instance HasText' Question where
  getText (OptionsQuestion' q) = q.text
  getText (MultiChoiceQuestion' q) = q.text
  getText (ListQuestion' q) = q.text
  getText (ValueQuestion' q) = q.text
  getText (IntegrationQuestion' q) = q.text
  setText (OptionsQuestion' q) newValue = OptionsQuestion' $ q {text = newValue}
  setText (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {text = newValue}
  setText (ListQuestion' q) newValue = ListQuestion' $ q {text = newValue}
  setText (ValueQuestion' q) newValue = ValueQuestion' $ q {text = newValue}
  setText (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {text = newValue}

------------------------------------------------------------------------------------------
instance HasRequiredPhaseUuid' Question where
  getRequiredPhaseUuid (OptionsQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (MultiChoiceQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (ListQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (ValueQuestion' q) = q.requiredPhaseUuid
  getRequiredPhaseUuid (IntegrationQuestion' q) = q.requiredPhaseUuid
  setRequiredPhaseUuid (OptionsQuestion' q) newValue = OptionsQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (ListQuestion' q) newValue = ListQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (ValueQuestion' q) newValue = ValueQuestion' $ q {requiredPhaseUuid = newValue}
  setRequiredPhaseUuid (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {requiredPhaseUuid = newValue}

------------------------------------------------------------------------------------------
instance HasTagUuids' Question where
  getTagUuids (OptionsQuestion' q) = q.tagUuids
  getTagUuids (MultiChoiceQuestion' q) = q.tagUuids
  getTagUuids (ListQuestion' q) = q.tagUuids
  getTagUuids (ValueQuestion' q) = q.tagUuids
  getTagUuids (IntegrationQuestion' q) = q.tagUuids
  setTagUuids (OptionsQuestion' q) newValue = OptionsQuestion' $ q {tagUuids = newValue}
  setTagUuids (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {tagUuids = newValue}
  setTagUuids (ListQuestion' q) newValue = ListQuestion' $ q {tagUuids = newValue}
  setTagUuids (ValueQuestion' q) newValue = ValueQuestion' $ q {tagUuids = newValue}
  setTagUuids (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {tagUuids = newValue}

-- ------------------------------------------------------------------------------------------
instance HasExpertUuids' Question [U.UUID] where
  getExpertUuids (OptionsQuestion' entity) = entity.expertUuids
  getExpertUuids (MultiChoiceQuestion' entity) = entity.expertUuids
  getExpertUuids (ListQuestion' entity) = entity.expertUuids
  getExpertUuids (ValueQuestion' entity) = entity.expertUuids
  getExpertUuids (IntegrationQuestion' entity) = entity.expertUuids
  setExpertUuids (OptionsQuestion' entity) newValue = OptionsQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (MultiChoiceQuestion' entity) newValue = MultiChoiceQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (ListQuestion' entity) newValue = ListQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (ValueQuestion' entity) newValue = ValueQuestion' $ entity {expertUuids = newValue}
  setExpertUuids (IntegrationQuestion' entity) newValue = IntegrationQuestion' $ entity {expertUuids = newValue}

-- ------------------------------------------------------------------------------------------
instance HasReferenceUuids' Question [U.UUID] where
  getReferenceUuids (OptionsQuestion' q) = q.referenceUuids
  getReferenceUuids (MultiChoiceQuestion' q) = q.referenceUuids
  getReferenceUuids (ListQuestion' q) = q.referenceUuids
  getReferenceUuids (ValueQuestion' q) = q.referenceUuids
  getReferenceUuids (IntegrationQuestion' q) = q.referenceUuids
  setReferenceUuids (OptionsQuestion' q) newValue = OptionsQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (MultiChoiceQuestion' q) newValue = MultiChoiceQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (ListQuestion' q) newValue = ListQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (ValueQuestion' q) newValue = ValueQuestion' $ q {referenceUuids = newValue}
  setReferenceUuids (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {referenceUuids = newValue}

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
instance HasProps' Question (M.Map String String) where
  getProps (IntegrationQuestion' q) = q.props
  getProps q = M.empty
  setProps (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q {props = newValue}
  setProps q newValue = q

instance HasProps' Integration [String] where
  getProps (ApiIntegration' integration) = integration.props
  getProps (WidgetIntegration' integration) = integration.props
  setProps (ApiIntegration' integration) newValue = ApiIntegration' $ integration {props = newValue}
  setProps (WidgetIntegration' integration) newValue = WidgetIntegration' $ integration {props = newValue}
