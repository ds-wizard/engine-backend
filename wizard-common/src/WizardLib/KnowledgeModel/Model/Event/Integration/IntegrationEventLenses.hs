module WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEventLenses where

import Shared.Common.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent

instance HasUuid' AddIntegrationEvent where
  getUuid (AddApiIntegrationEvent' entity) = entity.uuid
  getUuid (AddWidgetIntegrationEvent' entity) = entity.uuid
  setUuid (AddApiIntegrationEvent' entity) newValue = AddApiIntegrationEvent' $ entity {uuid = newValue}
  setUuid (AddWidgetIntegrationEvent' entity) newValue = AddWidgetIntegrationEvent' $ entity {uuid = newValue}

instance HasUuid' EditIntegrationEvent where
  getUuid (EditApiIntegrationEvent' entity) = entity.uuid
  getUuid (EditWidgetIntegrationEvent' entity) = entity.uuid
  setUuid (EditApiIntegrationEvent' entity) newValue = EditApiIntegrationEvent' $ entity {uuid = newValue}
  setUuid (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity {uuid = newValue}

instance HasUuid' DeleteIntegrationEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddIntegrationEvent where
  getParentUuid (AddApiIntegrationEvent' entity) = entity.parentUuid
  getParentUuid (AddWidgetIntegrationEvent' entity) = entity.parentUuid
  setParentUuid (AddApiIntegrationEvent' entity) newValue = AddApiIntegrationEvent' $ entity {parentUuid = newValue}
  setParentUuid (AddWidgetIntegrationEvent' entity) newValue = AddWidgetIntegrationEvent' $ entity {parentUuid = newValue}

instance HasParentUuid' EditIntegrationEvent where
  getParentUuid (EditApiIntegrationEvent' entity) = entity.parentUuid
  getParentUuid (EditWidgetIntegrationEvent' entity) = entity.parentUuid
  setParentUuid (EditApiIntegrationEvent' entity) newValue = EditApiIntegrationEvent' $ entity {parentUuid = newValue}
  setParentUuid (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity {parentUuid = newValue}

instance HasParentUuid' DeleteIntegrationEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddIntegrationEvent where
  getEntityUuid (AddApiIntegrationEvent' entity) = entity.entityUuid
  getEntityUuid (AddWidgetIntegrationEvent' entity) = entity.entityUuid
  setEntityUuid (AddApiIntegrationEvent' entity) newValue = AddApiIntegrationEvent' $ entity {entityUuid = newValue}
  setEntityUuid (AddWidgetIntegrationEvent' entity) newValue = AddWidgetIntegrationEvent' $ entity {entityUuid = newValue}

instance HasEntityUuid' EditIntegrationEvent where
  getEntityUuid (EditApiIntegrationEvent' entity) = entity.entityUuid
  getEntityUuid (EditWidgetIntegrationEvent' entity) = entity.entityUuid
  setEntityUuid (EditApiIntegrationEvent' entity) newValue = EditApiIntegrationEvent' $ entity {entityUuid = newValue}
  setEntityUuid (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity {entityUuid = newValue}

instance HasEntityUuid' DeleteIntegrationEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasProps' EditIntegrationEvent (EventField [String]) where
  getProps (EditApiIntegrationEvent' e) = e.props
  getProps (EditWidgetIntegrationEvent' e) = e.props
  setProps (EditApiIntegrationEvent' e) newValue = EditApiIntegrationEvent' $ e {props = newValue}
  setProps (EditWidgetIntegrationEvent' e) newValue = EditWidgetIntegrationEvent' $ e {props = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddIntegrationEvent where
  getCreatedAt (AddApiIntegrationEvent' entity) = entity.createdAt
  getCreatedAt (AddWidgetIntegrationEvent' entity) = entity.createdAt
  setCreatedAt (AddApiIntegrationEvent' entity) newValue = AddApiIntegrationEvent' $ entity {createdAt = newValue}
  setCreatedAt (AddWidgetIntegrationEvent' entity) newValue = AddWidgetIntegrationEvent' $ entity {createdAt = newValue}

instance HasCreatedAt' EditIntegrationEvent where
  getCreatedAt (EditApiIntegrationEvent' entity) = entity.createdAt
  getCreatedAt (EditWidgetIntegrationEvent' entity) = entity.createdAt
  setCreatedAt (EditApiIntegrationEvent' entity) newValue = EditApiIntegrationEvent' $ entity {createdAt = newValue}
  setCreatedAt (EditWidgetIntegrationEvent' entity) newValue = EditWidgetIntegrationEvent' $ entity {createdAt = newValue}

instance HasCreatedAt' DeleteIntegrationEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}
