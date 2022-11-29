module Shared.Model.Event.Reference.ReferenceEventLenses where

import Shared.Model.Common.Lens
import Shared.Model.Event.Reference.ReferenceEvent

instance HasUuid' AddReferenceEvent where
  getUuid (AddResourcePageReferenceEvent' entity) = entity.uuid
  getUuid (AddURLReferenceEvent' entity) = entity.uuid
  getUuid (AddCrossReferenceEvent' entity) = entity.uuid
  setUuid (AddResourcePageReferenceEvent' entity) newValue = AddResourcePageReferenceEvent' $ entity {uuid = newValue}
  setUuid (AddURLReferenceEvent' entity) newValue = AddURLReferenceEvent' $ entity {uuid = newValue}
  setUuid (AddCrossReferenceEvent' entity) newValue = AddCrossReferenceEvent' $ entity {uuid = newValue}

instance HasUuid' EditReferenceEvent where
  getUuid (EditResourcePageReferenceEvent' entity) = entity.uuid
  getUuid (EditURLReferenceEvent' entity) = entity.uuid
  getUuid (EditCrossReferenceEvent' entity) = entity.uuid
  setUuid (EditResourcePageReferenceEvent' entity) newValue =
    EditResourcePageReferenceEvent' $ entity {uuid = newValue}
  setUuid (EditURLReferenceEvent' entity) newValue = EditURLReferenceEvent' $ entity {uuid = newValue}
  setUuid (EditCrossReferenceEvent' entity) newValue = EditCrossReferenceEvent' $ entity {uuid = newValue}

instance HasUuid' DeleteReferenceEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddReferenceEvent where
  getParentUuid (AddResourcePageReferenceEvent' entity) = entity.parentUuid
  getParentUuid (AddURLReferenceEvent' entity) = entity.parentUuid
  getParentUuid (AddCrossReferenceEvent' entity) = entity.parentUuid
  setParentUuid (AddResourcePageReferenceEvent' entity) newValue =
    AddResourcePageReferenceEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddURLReferenceEvent' entity) newValue = AddURLReferenceEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddCrossReferenceEvent' entity) newValue = AddCrossReferenceEvent' $ entity {parentUuid = newValue}

instance HasParentUuid' EditReferenceEvent where
  getParentUuid (EditResourcePageReferenceEvent' entity) = entity.parentUuid
  getParentUuid (EditURLReferenceEvent' entity) = entity.parentUuid
  getParentUuid (EditCrossReferenceEvent' entity) = entity.parentUuid
  setParentUuid (EditResourcePageReferenceEvent' entity) newValue =
    EditResourcePageReferenceEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditURLReferenceEvent' entity) newValue = EditURLReferenceEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditCrossReferenceEvent' entity) newValue = EditCrossReferenceEvent' $ entity {parentUuid = newValue}

instance HasParentUuid' DeleteReferenceEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddReferenceEvent where
  getEntityUuid (AddResourcePageReferenceEvent' entity) = entity.entityUuid
  getEntityUuid (AddURLReferenceEvent' entity) = entity.entityUuid
  getEntityUuid (AddCrossReferenceEvent' entity) = entity.entityUuid
  setEntityUuid (AddResourcePageReferenceEvent' entity) newValue =
    AddResourcePageReferenceEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddURLReferenceEvent' entity) newValue = AddURLReferenceEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddCrossReferenceEvent' entity) newValue = AddCrossReferenceEvent' $ entity {entityUuid = newValue}

instance HasEntityUuid' EditReferenceEvent where
  getEntityUuid (EditResourcePageReferenceEvent' entity) = entity.entityUuid
  getEntityUuid (EditURLReferenceEvent' entity) = entity.entityUuid
  getEntityUuid (EditCrossReferenceEvent' entity) = entity.entityUuid
  setEntityUuid (EditResourcePageReferenceEvent' entity) newValue =
    EditResourcePageReferenceEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditURLReferenceEvent' entity) newValue = EditURLReferenceEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditCrossReferenceEvent' entity) newValue = EditCrossReferenceEvent' $ entity {entityUuid = newValue}

instance HasEntityUuid' DeleteReferenceEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddReferenceEvent where
  getCreatedAt (AddResourcePageReferenceEvent' entity) = entity.createdAt
  getCreatedAt (AddURLReferenceEvent' entity) = entity.createdAt
  getCreatedAt (AddCrossReferenceEvent' entity) = entity.createdAt
  setCreatedAt (AddResourcePageReferenceEvent' entity) newValue =
    AddResourcePageReferenceEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddURLReferenceEvent' entity) newValue = AddURLReferenceEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddCrossReferenceEvent' entity) newValue = AddCrossReferenceEvent' $ entity {createdAt = newValue}

instance HasCreatedAt' EditReferenceEvent where
  getCreatedAt (EditResourcePageReferenceEvent' entity) = entity.createdAt
  getCreatedAt (EditURLReferenceEvent' entity) = entity.createdAt
  getCreatedAt (EditCrossReferenceEvent' entity) = entity.createdAt
  setCreatedAt (EditResourcePageReferenceEvent' entity) newValue =
    EditResourcePageReferenceEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditURLReferenceEvent' entity) newValue = EditURLReferenceEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditCrossReferenceEvent' entity) newValue = EditCrossReferenceEvent' $ entity {createdAt = newValue}

instance HasCreatedAt' DeleteReferenceEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}
