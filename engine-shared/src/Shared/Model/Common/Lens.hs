module Shared.Model.Common.Lens where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel

class HasUuid' entity where
  getUuid :: entity -> U.UUID
  setUuid :: entity -> U.UUID -> entity

class HasEntityUuid' entity where
  getEntityUuid :: entity -> U.UUID
  setEntityUuid :: entity -> U.UUID -> entity

class HasParentUuid' entity where
  getParentUuid :: entity -> U.UUID
  setParentUuid :: entity -> U.UUID -> entity

class HasAnnotations' entity where
  getAnnotations :: entity -> [MapEntry String String]
  setAnnotations :: entity -> [MapEntry String String] -> entity

class HasCreatedAt' entity where
  getCreatedAt :: entity -> UTCTime
  setCreatedAt :: entity -> UTCTime -> entity

class HasCreatedBy' entity where
  getCreatedBy :: entity -> Maybe U.UUID
  setCreatedBy :: entity -> Maybe U.UUID -> entity

class HasExpertUuids' entity fieldType where
  getExpertUuids :: entity -> fieldType
  setExpertUuids :: entity -> fieldType -> entity

class HasReferenceUuids' entity fieldType where
  getReferenceUuids :: entity -> fieldType
  setReferenceUuids :: entity -> fieldType -> entity

class HasAnswerUuids' entity where
  getAnswerUuids :: entity -> [U.UUID]
  setAnswerUuids :: entity -> [U.UUID] -> entity

class HasTitle' entity where
  getTitle :: entity -> String
  setTitle :: entity -> String -> entity

class HasText' entity where
  getText :: entity -> Maybe String
  setText :: entity -> Maybe String -> entity

class HasRequiredPhaseUuid' entity where
  getRequiredPhaseUuid :: entity -> Maybe U.UUID
  setRequiredPhaseUuid :: entity -> Maybe U.UUID -> entity

class HasTagUuids' entity where
  getTagUuids :: entity -> [U.UUID]
  setTagUuids :: entity -> [U.UUID] -> entity

class HasChoiceUuids' entity where
  getChoiceUuids :: entity -> [U.UUID]
  setChoiceUuids :: entity -> [U.UUID] -> entity

class HasItemTemplateQuestionUuids' entity where
  getItemTemplateQuestionUuids :: entity -> [U.UUID]
  setItemTemplateQuestionUuids :: entity -> [U.UUID] -> entity

class HasValueType' entity where
  getValueType :: entity -> QuestionValueType
  setValueType :: entity -> QuestionValueType -> entity

class HasIntegrationUuid' entity where
  getIntegrationUuid :: entity -> U.UUID
  setIntegrationUuid :: entity -> U.UUID -> entity

class HasProps' entity fieldType where
  getProps :: entity -> fieldType
  setProps :: entity -> fieldType -> entity

class HasTargetUuid' entity where
  getTargetUuid :: entity -> U.UUID
  setTargetUuid :: entity -> U.UUID -> entity

-- ------------------------------------------------------------------------
class HasKnowledgeModelChapters entity where
  getChaptersL :: entity -> [Chapter]
  setChaptersL :: entity -> [Chapter] -> entity
  getChaptersM :: entity -> M.Map U.UUID Chapter
  setChaptersM :: entity -> M.Map U.UUID Chapter -> entity
  putInChaptersM :: U.UUID -> Chapter -> entity -> entity
  deleteFromChaptersM :: U.UUID -> entity -> entity

class HasKnowledgeModelQuestions entity where
  getQuestionsL :: entity -> [Question]
  setQuestionsL :: entity -> [Question] -> entity
  getQuestionsM :: entity -> M.Map U.UUID Question
  setQuestionsM :: entity -> M.Map U.UUID Question -> entity
  putInQuestionsM :: U.UUID -> Question -> entity -> entity
  deleteFromQuestionsM :: U.UUID -> entity -> entity

class HasKnowledgeModelAnswers entity where
  getAnswersL :: entity -> [Answer]
  setAnswersL :: entity -> [Answer] -> entity
  getAnswersM :: entity -> M.Map U.UUID Answer
  setAnswersM :: entity -> M.Map U.UUID Answer -> entity
  putInAnswersM :: U.UUID -> Answer -> entity -> entity
  deleteFromAnswersM :: U.UUID -> entity -> entity

class HasKnowledgeModelChoices entity where
  getChoicesL :: entity -> [Choice]
  setChoicesL :: entity -> [Choice] -> entity
  getChoicesM :: entity -> M.Map U.UUID Choice
  setChoicesM :: entity -> M.Map U.UUID Choice -> entity
  putInChoicesM :: U.UUID -> Choice -> entity -> entity
  deleteFromChoicesM :: U.UUID -> entity -> entity

class HasKnowledgeModelExperts entity where
  getExpertsL :: entity -> [Expert]
  setExpertsL :: entity -> [Expert] -> entity
  getExpertsM :: entity -> M.Map U.UUID Expert
  setExpertsM :: entity -> M.Map U.UUID Expert -> entity
  putInExpertsM :: U.UUID -> Expert -> entity -> entity
  deleteFromExpertsM :: U.UUID -> entity -> entity

class HasKnowledgeModelReferences entity where
  getReferencesL :: entity -> [Reference]
  setReferencesL :: entity -> [Reference] -> entity
  getReferencesM :: entity -> M.Map U.UUID Reference
  setReferencesM :: entity -> M.Map U.UUID Reference -> entity
  putInReferencesM :: U.UUID -> Reference -> entity -> entity
  deleteFromReferencesM :: U.UUID -> entity -> entity

class HasKnowledgeModelIntegrations entity where
  getIntegrationsL :: entity -> [Integration]
  setIntegrationsL :: entity -> [Integration] -> entity
  getIntegrationsM :: entity -> M.Map U.UUID Integration
  setIntegrationsM :: entity -> M.Map U.UUID Integration -> entity
  putInIntegrationsM :: U.UUID -> Integration -> entity -> entity
  deleteFromIntegrationsM :: U.UUID -> entity -> entity

class HasKnowledgeModelTags entity where
  getTagsL :: entity -> [Tag]
  setTagsL :: entity -> [Tag] -> entity
  getTagsM :: entity -> M.Map U.UUID Tag
  setTagsM :: entity -> M.Map U.UUID Tag -> entity
  putInTagsM :: U.UUID -> Tag -> entity -> entity
  deleteFromTagsM :: U.UUID -> entity -> entity

class HasKnowledgeModelMetrics entity where
  getMetricsL :: entity -> [Metric]
  setMetricsL :: entity -> [Metric] -> entity
  getMetricsM :: entity -> M.Map U.UUID Metric
  setMetricsM :: entity -> M.Map U.UUID Metric -> entity
  putInMetricsM :: U.UUID -> Metric -> entity -> entity
  deleteFromMetricsM :: U.UUID -> entity -> entity

class HasKnowledgeModelPhases entity where
  getPhasesL :: entity -> [Phase]
  setPhasesL :: entity -> [Phase] -> entity
  getPhasesM :: entity -> M.Map U.UUID Phase
  setPhasesM :: entity -> M.Map U.UUID Phase -> entity
  putInPhasesM :: U.UUID -> Phase -> entity -> entity
  deleteFromPhasesM :: U.UUID -> entity -> entity
