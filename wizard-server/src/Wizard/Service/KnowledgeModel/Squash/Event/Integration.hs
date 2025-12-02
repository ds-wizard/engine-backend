module Wizard.Service.KnowledgeModel.Squash.Event.Integration where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditIntegrationEvent where
  isSimpleEventSquashApplicable _ = True
  isReorderEventSquashApplicable _ _ = False

  --  --------------------------------------
  isTypeChanged (EditApiIntegrationEvent' oldEvent) (EditApiIntegrationEvent' newEvent) = False
  isTypeChanged (EditApiLegacyIntegrationEvent' oldEvent) (EditApiLegacyIntegrationEvent' newEvent) = False
  isTypeChanged (EditWidgetIntegrationEvent' oldEvent) (EditWidgetIntegrationEvent' newEvent) = False
  isTypeChanged _ _ = True

  --  --------------------------------------
  simpleSquashEvent previousEvent (oldEvent, EditApiIntegrationEvent' oldContent) (newEvent, EditApiIntegrationEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditIntegrationEvent' $
        EditApiIntegrationEvent' $
          EditApiIntegrationEvent
            { name = applyValue oldContent newContent (.name)
            , variables = applyValue oldContent newContent (.variables)
            , allowCustomReply = applyValue oldContent newContent (.allowCustomReply)
            , requestMethod = applyValue oldContent newContent (.requestMethod)
            , requestUrl = applyValue oldContent newContent (.requestUrl)
            , requestHeaders = applyValue oldContent newContent (.requestHeaders)
            , requestBody = applyValue oldContent newContent (.requestBody)
            , requestAllowEmptySearch = applyValue oldContent newContent (.requestAllowEmptySearch)
            , responseListField = applyValue oldContent newContent (.responseListField)
            , responseItemTemplate = applyValue oldContent newContent (.responseItemTemplate)
            , responseItemTemplateForSelection = applyValue oldContent newContent (.responseItemTemplateForSelection)
            , testQ = applyValue oldContent newContent (.testQ)
            , testVariables = applyValue oldContent newContent (.testVariables)
            , testResponse = applyValue oldContent newContent (.testResponse)
            , annotations = applyValue oldContent newContent (.annotations)
            }
  simpleSquashEvent previousEvent (oldEvent, EditApiLegacyIntegrationEvent' oldContent) (newEvent, EditApiLegacyIntegrationEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditIntegrationEvent' $
        EditApiLegacyIntegrationEvent' $
          EditApiLegacyIntegrationEvent
            { iId = applyValue oldContent newContent (.iId)
            , name = applyValue oldContent newContent (.name)
            , variables = applyValue oldContent newContent (.variables)
            , logo = applyValue oldContent newContent (.logo)
            , requestMethod = applyValue oldContent newContent (.requestMethod)
            , requestUrl = applyValue oldContent newContent (.requestUrl)
            , requestHeaders = applyValue oldContent newContent (.requestHeaders)
            , requestBody = applyValue oldContent newContent (.requestBody)
            , requestEmptySearch = applyValue oldContent newContent (.requestEmptySearch)
            , responseListField = applyValue oldContent newContent (.responseListField)
            , responseItemId = applyValue oldContent newContent (.responseItemId)
            , responseItemTemplate = applyValue oldContent newContent (.responseItemTemplate)
            , itemUrl = applyValue oldContent newContent (.itemUrl)
            , annotations = applyValue oldContent newContent (.annotations)
            }
  simpleSquashEvent previousEvent (oldEvent, EditWidgetIntegrationEvent' oldContent) (newEvent, EditWidgetIntegrationEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditIntegrationEvent' $
        EditWidgetIntegrationEvent' $
          EditWidgetIntegrationEvent
            { iId = applyValue oldContent newContent (.iId)
            , name = applyValue oldContent newContent (.name)
            , variables = applyValue oldContent newContent (.variables)
            , logo = applyValue oldContent newContent (.logo)
            , widgetUrl = applyValue oldContent newContent (.widgetUrl)
            , itemUrl = applyValue oldContent newContent (.itemUrl)
            , annotations = applyValue oldContent newContent (.annotations)
            }
  simpleSquashEvent previousEvent oldEvent newEvent = error $ "Simple squash event is not applicable for " <> show (oldEvent, newEvent) <> " in " <> show previousEvent
