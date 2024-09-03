module WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEventLenses where

import Shared.Common.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEvent

instance HasUuid' AddResourceCollectionEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' EditResourceCollectionEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' DeleteResourceCollectionEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
instance HasParentUuid' AddResourceCollectionEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' EditResourceCollectionEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' DeleteResourceCollectionEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

------------------------------------------------------------------------------------------
instance HasEntityUuid' AddResourceCollectionEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' EditResourceCollectionEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' DeleteResourceCollectionEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

------------------------------------------------------------------------------------------
instance HasCreatedAt' AddResourceCollectionEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' EditResourceCollectionEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' DeleteResourceCollectionEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasUuid' AddResourcePageEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' EditResourcePageEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' DeleteResourcePageEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
instance HasParentUuid' AddResourcePageEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' EditResourcePageEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' DeleteResourcePageEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

------------------------------------------------------------------------------------------
instance HasEntityUuid' AddResourcePageEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' EditResourcePageEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' DeleteResourcePageEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

------------------------------------------------------------------------------------------
instance HasCreatedAt' AddResourcePageEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' EditResourcePageEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' DeleteResourcePageEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}
