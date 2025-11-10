module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEventLenses where

import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasExpertUuids' EditQuestionEvent (EventField [U.UUID]) where
  getExpertUuids (EditOptionsQuestionEvent' entity) = entity.expertUuids
  getExpertUuids (EditMultiChoiceQuestionEvent' entity) = entity.expertUuids
  getExpertUuids (EditListQuestionEvent' entity) = entity.expertUuids
  getExpertUuids (EditValueQuestionEvent' entity) = entity.expertUuids
  getExpertUuids (EditIntegrationQuestionEvent' entity) = entity.expertUuids
  getExpertUuids (EditItemSelectQuestionEvent' entity) = entity.expertUuids
  getExpertUuids (EditFileQuestionEvent' entity) = entity.expertUuids
  setExpertUuids (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity {expertUuids = newValue}
  setExpertUuids (EditMultiChoiceQuestionEvent' entity) newValue = EditMultiChoiceQuestionEvent' $ entity {expertUuids = newValue}
  setExpertUuids (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity {expertUuids = newValue}
  setExpertUuids (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity {expertUuids = newValue}
  setExpertUuids (EditIntegrationQuestionEvent' entity) newValue = EditIntegrationQuestionEvent' $ entity {expertUuids = newValue}
  setExpertUuids (EditItemSelectQuestionEvent' entity) newValue = EditItemSelectQuestionEvent' $ entity {expertUuids = newValue}
  setExpertUuids (EditFileQuestionEvent' entity) newValue = EditFileQuestionEvent' $ entity {expertUuids = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasReferenceUuids' EditQuestionEvent (EventField [U.UUID]) where
  getReferenceUuids (EditOptionsQuestionEvent' entity) = entity.referenceUuids
  getReferenceUuids (EditMultiChoiceQuestionEvent' entity) = entity.referenceUuids
  getReferenceUuids (EditListQuestionEvent' entity) = entity.referenceUuids
  getReferenceUuids (EditValueQuestionEvent' entity) = entity.referenceUuids
  getReferenceUuids (EditIntegrationQuestionEvent' entity) = entity.referenceUuids
  getReferenceUuids (EditItemSelectQuestionEvent' entity) = entity.referenceUuids
  getReferenceUuids (EditFileQuestionEvent' entity) = entity.referenceUuids
  setReferenceUuids (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity {referenceUuids = newValue}
  setReferenceUuids (EditMultiChoiceQuestionEvent' entity) newValue = EditMultiChoiceQuestionEvent' $ entity {referenceUuids = newValue}
  setReferenceUuids (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity {referenceUuids = newValue}
  setReferenceUuids (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity {referenceUuids = newValue}
  setReferenceUuids (EditIntegrationQuestionEvent' entity) newValue = EditIntegrationQuestionEvent' $ entity {referenceUuids = newValue}
  setReferenceUuids (EditItemSelectQuestionEvent' entity) newValue = EditItemSelectQuestionEvent' $ entity {referenceUuids = newValue}
  setReferenceUuids (EditFileQuestionEvent' entity) newValue = EditFileQuestionEvent' $ entity {referenceUuids = newValue}
