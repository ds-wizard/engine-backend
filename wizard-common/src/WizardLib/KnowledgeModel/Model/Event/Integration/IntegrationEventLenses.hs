module WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEventLenses where

import Shared.Common.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent

instance HasUuid' AddIntegrationEvent where
  getUuid (AddApiLegacyIntegrationEvent' entity) = entity.uuid
  getUuid (AddWidgetIntegrationEvent' entity) = entity.uuid
  setUuid (AddApiLegacyIntegrationEvent' entity) newValue = AddApiLegacyIntegrationEvent' $ entity {uuid = newValue}
  setUuid (AddWidgetIntegrationEvent' entity) newValue = AddWidgetIntegrationEvent' $ entity {uuid = newValue}

instance HasUuid' EditIntegrationEvent where
  getUuid (EditApiLegacyIntegrationEvent' entity) = entity.uuid
  getUuid (EditWidgetIntegrationEvent' entity) = entity.uuid
  setUuid (EditApiLegacyIntegrationEvent' entity) newValue = EditApiLegacyIntegrationEvent' $ entity {uuid = newValue}
  setUuid (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity {uuid = newValue}

instance HasUuid' DeleteIntegrationEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddIntegrationEvent where
  getParentUuid (AddApiLegacyIntegrationEvent' entity) = entity.parentUuid
  getParentUuid (AddWidgetIntegrationEvent' entity) = entity.parentUuid
  setParentUuid (AddApiLegacyIntegrationEvent' entity) newValue = AddApiLegacyIntegrationEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddWidgetIntegrationEvent' entity) newValue = AddWidgetIntegrationEvent' $ entity {parentUuid = newValue}

instance HasParentUuid' EditIntegrationEvent where
  getParentUuid (EditApiLegacyIntegrationEvent' entity) = entity.parentUuid
  getParentUuid (EditWidgetIntegrationEvent' entity) = entity.parentUuid
  setParentUuid (EditApiLegacyIntegrationEvent' entity) newValue = EditApiLegacyIntegrationEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity {parentUuid = newValue}

instance HasParentUuid' DeleteIntegrationEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddIntegrationEvent where
  getEntityUuid (AddApiLegacyIntegrationEvent' entity) = entity.entityUuid
  getEntityUuid (AddWidgetIntegrationEvent' entity) = entity.entityUuid
  setEntityUuid (AddApiLegacyIntegrationEvent' entity) newValue = AddApiLegacyIntegrationEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddWidgetIntegrationEvent' entity) newValue = AddWidgetIntegrationEvent' $ entity {entityUuid = newValue}

instance HasEntityUuid' EditIntegrationEvent where
  getEntityUuid (EditApiLegacyIntegrationEvent' entity) = entity.entityUuid
  getEntityUuid (EditWidgetIntegrationEvent' entity) = entity.entityUuid
  setEntityUuid (EditApiLegacyIntegrationEvent' entity) newValue = EditApiLegacyIntegrationEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity {entityUuid = newValue}

instance HasEntityUuid' DeleteIntegrationEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasProps' EditIntegrationEvent (EventField [String]) where
  getProps (EditApiLegacyIntegrationEvent' e) = e.props
  getProps (EditWidgetIntegrationEvent' e) = e.props
  setProps (EditApiLegacyIntegrationEvent' e) newValue = EditApiLegacyIntegrationEvent' $ e {props = newValue}
  setProps (EditWidgetIntegrationEvent' e) newValue = EditWidgetIntegrationEvent' $ e {props = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddIntegrationEvent where
  getCreatedAt (AddApiLegacyIntegrationEvent' entity) = entity.createdAt
  getCreatedAt (AddWidgetIntegrationEvent' entity) = entity.createdAt
  setCreatedAt (AddApiLegacyIntegrationEvent' entity) newValue = AddApiLegacyIntegrationEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddWidgetIntegrationEvent' entity) newValue = AddWidgetIntegrationEvent' $ entity {createdAt = newValue}

instance HasCreatedAt' EditIntegrationEvent where
  getCreatedAt (EditApiLegacyIntegrationEvent' entity) = entity.createdAt
  getCreatedAt (EditWidgetIntegrationEvent' entity) = entity.createdAt
  setCreatedAt (EditApiLegacyIntegrationEvent' entity) newValue = EditApiLegacyIntegrationEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity {createdAt = newValue}

instance HasCreatedAt' DeleteIntegrationEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}
