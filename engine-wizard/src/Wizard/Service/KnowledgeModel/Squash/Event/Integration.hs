module Wizard.Service.KnowledgeModel.Squash.Event.Integration where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Integration.IntegrationEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditIntegrationEvent where
  isSimpleEventSquashApplicable _ = True
  --  --------------------------------------
  isTypeChanged (EditApiIntegrationEvent' oldEvent) (EditApiIntegrationEvent' newEvent) = False
  isTypeChanged (EditWidgetIntegrationEvent' oldEvent) (EditWidgetIntegrationEvent' newEvent) = False
  isTypeChanged _ _ = True
  --  --------------------------------------
  simpleSquashEvent (EditApiIntegrationEvent' oldEvent) (EditApiIntegrationEvent' newEvent) =
    EditApiIntegrationEvent' $
    EditApiIntegrationEvent
      { _editApiIntegrationEventUuid = newEvent ^. uuid
      , _editApiIntegrationEventParentUuid = newEvent ^. parentUuid
      , _editApiIntegrationEventEntityUuid = newEvent ^. entityUuid
      , _editApiIntegrationEventIId = applyValue oldEvent newEvent iId
      , _editApiIntegrationEventName = applyValue oldEvent newEvent name
      , _editApiIntegrationEventProps = applyValue oldEvent newEvent props
      , _editApiIntegrationEventLogo = applyValue oldEvent newEvent logo
      , _editApiIntegrationEventRequestMethod = applyValue oldEvent newEvent requestMethod
      , _editApiIntegrationEventRequestUrl = applyValue oldEvent newEvent requestUrl
      , _editApiIntegrationEventRequestHeaders = applyValue oldEvent newEvent requestHeaders
      , _editApiIntegrationEventRequestBody = applyValue oldEvent newEvent requestBody
      , _editApiIntegrationEventRequestEmptySearch = applyValue oldEvent newEvent requestEmptySearch
      , _editApiIntegrationEventResponseListField = applyValue oldEvent newEvent responseListField
      , _editApiIntegrationEventResponseItemId = applyValue oldEvent newEvent responseItemId
      , _editApiIntegrationEventResponseItemTemplate = applyValue oldEvent newEvent responseItemTemplate
      , _editApiIntegrationEventItemUrl = applyValue oldEvent newEvent itemUrl
      , _editApiIntegrationEventAnnotations = applyValue oldEvent newEvent annotations
      , _editApiIntegrationEventCreatedAt = newEvent ^. createdAt
      }
  simpleSquashEvent (EditWidgetIntegrationEvent' oldEvent) (EditWidgetIntegrationEvent' newEvent) =
    EditWidgetIntegrationEvent' $
    EditWidgetIntegrationEvent
      { _editWidgetIntegrationEventUuid = newEvent ^. uuid
      , _editWidgetIntegrationEventParentUuid = newEvent ^. parentUuid
      , _editWidgetIntegrationEventEntityUuid = newEvent ^. entityUuid
      , _editWidgetIntegrationEventIId = applyValue oldEvent newEvent iId
      , _editWidgetIntegrationEventName = applyValue oldEvent newEvent name
      , _editWidgetIntegrationEventProps = applyValue oldEvent newEvent props
      , _editWidgetIntegrationEventLogo = applyValue oldEvent newEvent logo
      , _editWidgetIntegrationEventWidgetUrl = applyValue oldEvent newEvent widgetUrl
      , _editWidgetIntegrationEventItemUrl = applyValue oldEvent newEvent itemUrl
      , _editWidgetIntegrationEventAnnotations = applyValue oldEvent newEvent annotations
      , _editWidgetIntegrationEventCreatedAt = newEvent ^. createdAt
      }
