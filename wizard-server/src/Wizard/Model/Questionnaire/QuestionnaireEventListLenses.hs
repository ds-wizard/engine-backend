module Wizard.Model.Questionnaire.QuestionnaireEventListLenses where

import Shared.Common.Model.Common.Lens
import Wizard.Model.Questionnaire.QuestionnaireEventList

instance HasUuid' QuestionnaireEventList where
  getUuid (SetReplyEventList' entity) = entity.uuid
  getUuid (ClearReplyEventList' entity) = entity.uuid
  getUuid (SetPhaseEventList' entity) = entity.uuid
  getUuid (SetLabelsEventList' entity) = entity.uuid
  setUuid (SetReplyEventList' entity) newValue = SetReplyEventList' $ entity {uuid = newValue}
  setUuid (ClearReplyEventList' entity) newValue = ClearReplyEventList' $ entity {uuid = newValue}
  setUuid (SetPhaseEventList' entity) newValue = SetPhaseEventList' $ entity {uuid = newValue}
  setUuid (SetLabelsEventList' entity) newValue = SetLabelsEventList' $ entity {uuid = newValue}

instance HasCreatedAt' QuestionnaireEventList where
  getCreatedAt (SetReplyEventList' entity) = entity.createdAt
  getCreatedAt (ClearReplyEventList' entity) = entity.createdAt
  getCreatedAt (SetPhaseEventList' entity) = entity.createdAt
  getCreatedAt (SetLabelsEventList' entity) = entity.createdAt
  setCreatedAt (SetReplyEventList' entity) newValue = SetReplyEventList' $ entity {createdAt = newValue}
  setCreatedAt (ClearReplyEventList' entity) newValue = ClearReplyEventList' $ entity {createdAt = newValue}
  setCreatedAt (SetPhaseEventList' entity) newValue = SetPhaseEventList' $ entity {createdAt = newValue}
  setCreatedAt (SetLabelsEventList' entity) newValue = SetLabelsEventList' $ entity {createdAt = newValue}
