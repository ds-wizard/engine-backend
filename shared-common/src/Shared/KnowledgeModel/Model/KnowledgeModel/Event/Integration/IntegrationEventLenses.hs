module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEventLenses where

import Shared.Common.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance HasVariables' EditIntegrationEvent (EventField [String]) where
  getVariables (EditApiIntegrationEvent' e) = e.variables
  getVariables (EditPluginIntegrationEvent' e) = NothingChanged
  setVariables (EditApiIntegrationEvent' e) newValue = EditApiIntegrationEvent' $ e {variables = newValue}
  setVariables (EditPluginIntegrationEvent' e) newValue = EditPluginIntegrationEvent' e
