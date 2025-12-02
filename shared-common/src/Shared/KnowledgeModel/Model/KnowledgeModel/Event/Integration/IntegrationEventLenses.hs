module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEventLenses where

import Shared.Common.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance HasVariables' EditIntegrationEvent (EventField [String]) where
  getVariables (EditApiIntegrationEvent' e) = e.variables
  getVariables (EditApiLegacyIntegrationEvent' e) = e.variables
  getVariables (EditWidgetIntegrationEvent' e) = e.variables
  setVariables (EditApiIntegrationEvent' e) newValue = EditApiIntegrationEvent' $ e {variables = newValue}
  setVariables (EditApiLegacyIntegrationEvent' e) newValue = EditApiLegacyIntegrationEvent' $ e {variables = newValue}
  setVariables (EditWidgetIntegrationEvent' e) newValue = EditWidgetIntegrationEvent' $ e {variables = newValue}
