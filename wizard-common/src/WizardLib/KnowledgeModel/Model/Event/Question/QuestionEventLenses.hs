module WizardLib.KnowledgeModel.Model.Event.Question.QuestionEventLenses where

import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent

instance HasUuid' AddQuestionEvent where
  getUuid (AddOptionsQuestionEvent' entity) = entity.uuid
  getUuid (AddMultiChoiceQuestionEvent' entity) = entity.uuid
  getUuid (AddListQuestionEvent' entity) = entity.uuid
  getUuid (AddValueQuestionEvent' entity) = entity.uuid
  getUuid (AddIntegrationQuestionEvent' entity) = entity.uuid
  getUuid (AddItemSelectQuestionEvent' entity) = entity.uuid
  getUuid (AddFileQuestionEvent' entity) = entity.uuid
  setUuid (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity {uuid = newValue}
  setUuid (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity {uuid = newValue}
  setUuid (AddMultiChoiceQuestionEvent' entity) newValue = AddMultiChoiceQuestionEvent' $ entity {uuid = newValue}
  setUuid (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity {uuid = newValue}
  setUuid (AddIntegrationQuestionEvent' entity) newValue = AddIntegrationQuestionEvent' $ entity {uuid = newValue}
  setUuid (AddItemSelectQuestionEvent' entity) newValue = AddItemSelectQuestionEvent' $ entity {uuid = newValue}
  setUuid (AddFileQuestionEvent' entity) newValue = AddFileQuestionEvent' $ entity {uuid = newValue}

instance HasUuid' EditQuestionEvent where
  getUuid (EditOptionsQuestionEvent' entity) = entity.uuid
  getUuid (EditMultiChoiceQuestionEvent' entity) = entity.uuid
  getUuid (EditListQuestionEvent' entity) = entity.uuid
  getUuid (EditValueQuestionEvent' entity) = entity.uuid
  getUuid (EditIntegrationQuestionEvent' entity) = entity.uuid
  getUuid (EditItemSelectQuestionEvent' entity) = entity.uuid
  getUuid (EditFileQuestionEvent' entity) = entity.uuid
  setUuid (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity {uuid = newValue}
  setUuid (EditMultiChoiceQuestionEvent' entity) newValue = EditMultiChoiceQuestionEvent' $ entity {uuid = newValue}
  setUuid (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity {uuid = newValue}
  setUuid (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity {uuid = newValue}
  setUuid (EditIntegrationQuestionEvent' entity) newValue = EditIntegrationQuestionEvent' $ entity {uuid = newValue}
  setUuid (EditItemSelectQuestionEvent' entity) newValue = EditItemSelectQuestionEvent' $ entity {uuid = newValue}
  setUuid (EditFileQuestionEvent' entity) newValue = EditFileQuestionEvent' $ entity {uuid = newValue}

instance HasUuid' DeleteQuestionEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddQuestionEvent where
  getParentUuid (AddOptionsQuestionEvent' entity) = entity.parentUuid
  getParentUuid (AddMultiChoiceQuestionEvent' entity) = entity.parentUuid
  getParentUuid (AddListQuestionEvent' entity) = entity.parentUuid
  getParentUuid (AddValueQuestionEvent' entity) = entity.parentUuid
  getParentUuid (AddIntegrationQuestionEvent' entity) = entity.parentUuid
  getParentUuid (AddItemSelectQuestionEvent' entity) = entity.parentUuid
  getParentUuid (AddFileQuestionEvent' entity) = entity.parentUuid
  setParentUuid (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddMultiChoiceQuestionEvent' entity) newValue = AddMultiChoiceQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddIntegrationQuestionEvent' entity) newValue = AddIntegrationQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddItemSelectQuestionEvent' entity) newValue = AddItemSelectQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddFileQuestionEvent' entity) newValue = AddFileQuestionEvent' $ entity {parentUuid = newValue}

instance HasParentUuid' EditQuestionEvent where
  getParentUuid (EditOptionsQuestionEvent' entity) = entity.parentUuid
  getParentUuid (EditMultiChoiceQuestionEvent' entity) = entity.parentUuid
  getParentUuid (EditListQuestionEvent' entity) = entity.parentUuid
  getParentUuid (EditValueQuestionEvent' entity) = entity.parentUuid
  getParentUuid (EditIntegrationQuestionEvent' entity) = entity.parentUuid
  getParentUuid (EditItemSelectQuestionEvent' entity) = entity.parentUuid
  getParentUuid (EditFileQuestionEvent' entity) = entity.parentUuid
  setParentUuid (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditMultiChoiceQuestionEvent' entity) newValue = EditMultiChoiceQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditIntegrationQuestionEvent' entity) newValue = EditIntegrationQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditItemSelectQuestionEvent' entity) newValue = EditItemSelectQuestionEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditFileQuestionEvent' entity) newValue = EditFileQuestionEvent' $ entity {parentUuid = newValue}

instance HasParentUuid' DeleteQuestionEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddQuestionEvent where
  getEntityUuid (AddOptionsQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (AddMultiChoiceQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (AddListQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (AddValueQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (AddIntegrationQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (AddItemSelectQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (AddFileQuestionEvent' entity) = entity.entityUuid
  setEntityUuid (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddMultiChoiceQuestionEvent' entity) newValue = AddMultiChoiceQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddIntegrationQuestionEvent' entity) newValue = AddIntegrationQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddItemSelectQuestionEvent' entity) newValue = AddItemSelectQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddFileQuestionEvent' entity) newValue = AddFileQuestionEvent' $ entity {entityUuid = newValue}

instance HasEntityUuid' EditQuestionEvent where
  getEntityUuid (EditOptionsQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (EditMultiChoiceQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (EditListQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (EditValueQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (EditIntegrationQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (EditItemSelectQuestionEvent' entity) = entity.entityUuid
  getEntityUuid (EditFileQuestionEvent' entity) = entity.entityUuid
  setEntityUuid (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditMultiChoiceQuestionEvent' entity) newValue = EditMultiChoiceQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditIntegrationQuestionEvent' entity) newValue = EditIntegrationQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditItemSelectQuestionEvent' entity) newValue = EditItemSelectQuestionEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditFileQuestionEvent' entity) newValue = EditFileQuestionEvent' $ entity {entityUuid = newValue}

instance HasEntityUuid' DeleteQuestionEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddQuestionEvent where
  getCreatedAt (AddOptionsQuestionEvent' entity) = entity.createdAt
  getCreatedAt (AddMultiChoiceQuestionEvent' entity) = entity.createdAt
  getCreatedAt (AddListQuestionEvent' entity) = entity.createdAt
  getCreatedAt (AddValueQuestionEvent' entity) = entity.createdAt
  getCreatedAt (AddIntegrationQuestionEvent' entity) = entity.createdAt
  getCreatedAt (AddItemSelectQuestionEvent' entity) = entity.createdAt
  getCreatedAt (AddFileQuestionEvent' entity) = entity.createdAt
  setCreatedAt (AddOptionsQuestionEvent' entity) newValue = AddOptionsQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddMultiChoiceQuestionEvent' entity) newValue = AddMultiChoiceQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddListQuestionEvent' entity) newValue = AddListQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddValueQuestionEvent' entity) newValue = AddValueQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddIntegrationQuestionEvent' entity) newValue = AddIntegrationQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddItemSelectQuestionEvent' entity) newValue = AddItemSelectQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddFileQuestionEvent' entity) newValue = AddFileQuestionEvent' $ entity {createdAt = newValue}

instance HasCreatedAt' EditQuestionEvent where
  getCreatedAt (EditOptionsQuestionEvent' entity) = entity.createdAt
  getCreatedAt (EditMultiChoiceQuestionEvent' entity) = entity.createdAt
  getCreatedAt (EditListQuestionEvent' entity) = entity.createdAt
  getCreatedAt (EditValueQuestionEvent' entity) = entity.createdAt
  getCreatedAt (EditIntegrationQuestionEvent' entity) = entity.createdAt
  getCreatedAt (EditItemSelectQuestionEvent' entity) = entity.createdAt
  getCreatedAt (EditFileQuestionEvent' entity) = entity.createdAt
  setCreatedAt (EditOptionsQuestionEvent' entity) newValue = EditOptionsQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditMultiChoiceQuestionEvent' entity) newValue = EditMultiChoiceQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditListQuestionEvent' entity) newValue = EditListQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditValueQuestionEvent' entity) newValue = EditValueQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditIntegrationQuestionEvent' entity) newValue = EditIntegrationQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditItemSelectQuestionEvent' entity) newValue = EditItemSelectQuestionEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditFileQuestionEvent' entity) newValue = EditFileQuestionEvent' $ entity {createdAt = newValue}

instance HasCreatedAt' DeleteQuestionEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

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
