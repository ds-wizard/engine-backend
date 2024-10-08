module Wizard.Service.KnowledgeModel.Squash.Event.Integration where

import Wizard.Service.KnowledgeModel.Squash.Event.Common
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent

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
        , iId = applyValue oldEvent newEvent (.iId)
        , name = applyValue oldEvent newEvent (.name)
        , props = applyValue oldEvent newEvent (.props)
        , logo = applyValue oldEvent newEvent (.logo)
        , requestMethod = applyValue oldEvent newEvent (.requestMethod)
        , requestUrl = applyValue oldEvent newEvent (.requestUrl)
        , requestHeaders = applyValue oldEvent newEvent (.requestHeaders)
        , requestBody = applyValue oldEvent newEvent (.requestBody)
        , requestEmptySearch = applyValue oldEvent newEvent (.requestEmptySearch)
        , responseListField = applyValue oldEvent newEvent (.responseListField)
        , responseItemId = applyValue oldEvent newEvent (.responseItemId)
        , responseItemTemplate = applyValue oldEvent newEvent (.responseItemTemplate)
        , itemUrl = applyValue oldEvent newEvent (.itemUrl)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , createdAt = oldEvent.createdAt
        }
  simpleSquashEvent previousEvent (EditWidgetIntegrationEvent' oldEvent) (EditWidgetIntegrationEvent' newEvent) =
    EditWidgetIntegrationEvent' $
      EditWidgetIntegrationEvent
        { uuid = newEvent.uuid
        , parentUuid = newEvent.parentUuid
        , entityUuid = newEvent.entityUuid
        , iId = applyValue oldEvent newEvent (.iId)
        , name = applyValue oldEvent newEvent (.name)
        , props = applyValue oldEvent newEvent (.props)
        , logo = applyValue oldEvent newEvent (.logo)
        , widgetUrl = applyValue oldEvent newEvent (.widgetUrl)
        , itemUrl = applyValue oldEvent newEvent (.itemUrl)
        , annotations = applyValue oldEvent newEvent (.annotations)
        , createdAt = oldEvent.createdAt
        }
