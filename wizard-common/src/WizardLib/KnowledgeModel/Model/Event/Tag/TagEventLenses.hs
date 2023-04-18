module WizardLib.KnowledgeModel.Model.Event.Tag.TagEventLenses where

import Shared.Common.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Tag.TagEvent

instance HasUuid' AddTagEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' EditTagEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' DeleteTagEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddTagEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' EditTagEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' DeleteTagEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddTagEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' EditTagEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' DeleteTagEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddTagEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' EditTagEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' DeleteTagEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}
