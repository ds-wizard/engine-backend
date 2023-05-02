module Wizard.Model.Questionnaire.QuestionnaireEventLenses where

import Shared.Common.Model.Common.Lens
import Wizard.Model.Questionnaire.QuestionnaireEvent

instance HasUuid' QuestionnaireEvent where
  getUuid (SetReplyEvent' entity) = entity.uuid
  getUuid (ClearReplyEvent' entity) = entity.uuid
  getUuid (SetPhaseEvent' entity) = entity.uuid
  getUuid (SetLabelsEvent' entity) = entity.uuid
  setUuid (SetReplyEvent' entity) newValue = SetReplyEvent' $ entity {uuid = newValue}
  setUuid (ClearReplyEvent' entity) newValue = ClearReplyEvent' $ entity {uuid = newValue}
  setUuid (SetPhaseEvent' entity) newValue = SetPhaseEvent' $ entity {uuid = newValue}
  setUuid (SetLabelsEvent' entity) newValue = SetLabelsEvent' $ entity {uuid = newValue}

instance HasCreatedAt' QuestionnaireEvent where
  getCreatedAt (SetReplyEvent' entity) = entity.createdAt
  getCreatedAt (ClearReplyEvent' entity) = entity.createdAt
  getCreatedAt (SetPhaseEvent' entity) = entity.createdAt
  getCreatedAt (SetLabelsEvent' entity) = entity.createdAt
  setCreatedAt (SetReplyEvent' entity) newValue = SetReplyEvent' $ entity {createdAt = newValue}
  setCreatedAt (ClearReplyEvent' entity) newValue = ClearReplyEvent' $ entity {createdAt = newValue}
  setCreatedAt (SetPhaseEvent' entity) newValue = SetPhaseEvent' $ entity {createdAt = newValue}
  setCreatedAt (SetLabelsEvent' entity) newValue = SetLabelsEvent' $ entity {createdAt = newValue}

instance HasCreatedBy' QuestionnaireEvent where
  getCreatedBy (SetReplyEvent' entity) = entity.createdBy
  getCreatedBy (ClearReplyEvent' entity) = entity.createdBy
  getCreatedBy (SetPhaseEvent' entity) = entity.createdBy
  getCreatedBy (SetLabelsEvent' entity) = entity.createdBy
  setCreatedBy (SetReplyEvent' entity) newValue = SetReplyEvent' $ entity {createdBy = newValue}
  setCreatedBy (ClearReplyEvent' entity) newValue = ClearReplyEvent' $ entity {createdBy = newValue}
  setCreatedBy (SetPhaseEvent' entity) newValue = SetPhaseEvent' $ entity {createdBy = newValue}
  setCreatedBy (SetLabelsEvent' entity) newValue = SetLabelsEvent' $ entity {createdBy = newValue}
