module Wizard.Service.KnowledgeModel.Squash.Event.Integration where

import Shared.Model.Common.MapEntry
import Shared.Model.Event.EventField
import Shared.Model.Event.Integration.IntegrationEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditIntegrationEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False

  --  --------------------------------------
  isTypeChanged (EditApiIntegrationEvent' oldEvent) (EditApiIntegrationEvent' newEvent) = False
  isTypeChanged (EditWidgetIntegrationEvent' oldEvent) (EditWidgetIntegrationEvent' newEvent) = False
  isTypeChanged _ _ = True

  --  --------------------------------------
  simpleSquashEvent previousEvent (EditApiIntegrationEvent' oldEvent) (EditApiIntegrationEvent' newEvent) =
    EditApiIntegrationEvent' $
      EditApiIntegrationEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , iId = applyValue oldEvent newEvent ((.iId) :: EditApiIntegrationEvent -> EventField String)
        , name = applyValue oldEvent newEvent ((.name) :: EditApiIntegrationEvent -> EventField String)
        , props = applyValue oldEvent newEvent ((.props) :: EditApiIntegrationEvent -> EventField [String])
        , logo = applyValue oldEvent newEvent ((.logo) :: EditApiIntegrationEvent -> EventField String)
        , requestMethod = applyValue oldEvent newEvent ((.requestMethod) :: EditApiIntegrationEvent -> EventField String)
        , requestUrl = applyValue oldEvent newEvent ((.requestUrl) :: EditApiIntegrationEvent -> EventField String)
        , requestHeaders = applyValue oldEvent newEvent ((.requestHeaders) :: EditApiIntegrationEvent -> EventField [MapEntry String String])
        , requestBody = applyValue oldEvent newEvent ((.requestBody) :: EditApiIntegrationEvent -> EventField String)
        , requestEmptySearch = applyValue oldEvent newEvent ((.requestEmptySearch) :: EditApiIntegrationEvent -> EventField Bool)
        , responseListField = applyValue oldEvent newEvent ((.responseListField) :: EditApiIntegrationEvent -> EventField String)
        , responseItemId = applyValue oldEvent newEvent ((.responseItemId) :: EditApiIntegrationEvent -> EventField String)
        , responseItemTemplate = applyValue oldEvent newEvent ((.responseItemTemplate) :: EditApiIntegrationEvent -> EventField String)
        , itemUrl = applyValue oldEvent newEvent ((.itemUrl) :: EditApiIntegrationEvent -> EventField String)
        , annotations = applyValue oldEvent newEvent ((.annotations) :: EditApiIntegrationEvent -> EventField [MapEntry String String])
        , createdAt = newEvent.createdAt
        }
  simpleSquashEvent previousEvent (EditWidgetIntegrationEvent' oldEvent) (EditWidgetIntegrationEvent' newEvent) =
    EditWidgetIntegrationEvent' $
      EditWidgetIntegrationEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , iId = applyValue oldEvent newEvent ((.iId) :: EditWidgetIntegrationEvent -> EventField String)
        , name = applyValue oldEvent newEvent ((.name) :: EditWidgetIntegrationEvent -> EventField String)
        , props = applyValue oldEvent newEvent ((.props) :: EditWidgetIntegrationEvent -> EventField [String])
        , logo = applyValue oldEvent newEvent ((.logo) :: EditWidgetIntegrationEvent -> EventField String)
        , widgetUrl = applyValue oldEvent newEvent ((.widgetUrl) :: EditWidgetIntegrationEvent -> EventField String)
        , itemUrl = applyValue oldEvent newEvent ((.itemUrl) :: EditWidgetIntegrationEvent -> EventField String)
        , annotations = applyValue oldEvent newEvent ((.annotations) :: EditWidgetIntegrationEvent -> EventField [MapEntry String String])
        , createdAt = newEvent.createdAt
        }
