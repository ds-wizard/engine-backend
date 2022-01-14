module Wizard.Service.KnowledgeModel.Squash.Event.Integration where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Integration.IntegrationEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditIntegrationEvent where
  isSimpleEventSquashApplicable _ = True
  isTypeChanged _ _ = False
  simpleSquashEvent oldEvent newEvent =
    EditIntegrationEvent
      { _editIntegrationEventUuid = newEvent ^. uuid
      , _editIntegrationEventParentUuid = newEvent ^. parentUuid
      , _editIntegrationEventEntityUuid = newEvent ^. entityUuid
      , _editIntegrationEventIId = applyValue oldEvent newEvent iId
      , _editIntegrationEventName = applyValue oldEvent newEvent name
      , _editIntegrationEventProps = applyValue oldEvent newEvent props
      , _editIntegrationEventLogo = applyValue oldEvent newEvent logo
      , _editIntegrationEventRequestMethod = applyValue oldEvent newEvent requestMethod
      , _editIntegrationEventRequestUrl = applyValue oldEvent newEvent requestUrl
      , _editIntegrationEventRequestHeaders = applyValue oldEvent newEvent requestHeaders
      , _editIntegrationEventRequestBody = applyValue oldEvent newEvent requestBody
      , _editIntegrationEventResponseListField = applyValue oldEvent newEvent responseListField
      , _editIntegrationEventResponseItemUrl = applyValue oldEvent newEvent responseItemUrl
      , _editIntegrationEventResponseItemId = applyValue oldEvent newEvent responseItemId
      , _editIntegrationEventResponseItemTemplate = applyValue oldEvent newEvent responseItemTemplate
      , _editIntegrationEventAnnotations = applyValue oldEvent newEvent annotations
      , _editIntegrationEventCreatedAt = newEvent ^. createdAt
      }
