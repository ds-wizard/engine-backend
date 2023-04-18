module Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration where

import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance CreateEntity AddIntegrationEvent Integration where
  createEntity (AddApiIntegrationEvent' event) =
    ApiIntegration' $
      ApiIntegration
        { uuid = event.entityUuid
        , iId = event.iId
        , name = event.name
        , props = event.props
        , logo = event.logo
        , requestMethod = event.requestMethod
        , requestUrl = event.requestUrl
        , requestHeaders = event.requestHeaders
        , requestBody = event.requestBody
        , requestEmptySearch = event.requestEmptySearch
        , responseListField = event.responseListField
        , responseItemId = event.responseItemId
        , responseItemTemplate = event.responseItemTemplate
        , itemUrl = event.itemUrl
        , annotations = event.annotations
        }
  createEntity (AddWidgetIntegrationEvent' event) =
    WidgetIntegration' $
      WidgetIntegration
        { uuid = event.entityUuid
        , iId = event.iId
        , name = event.name
        , props = event.props
        , logo = event.logo
        , widgetUrl = event.widgetUrl
        , itemUrl = event.itemUrl
        , annotations = event.annotations
        }

instance EditEntity EditIntegrationEvent Integration where
  editEntity event' integration =
    case event' of
      (EditApiIntegrationEvent' event) -> applyToApiIntegration event . convertToApiIntegration $ integration
      (EditWidgetIntegrationEvent' event) -> applyToWidgetIntegration event . convertToWidgetIntegration $ integration
    where
      applyToApiIntegration event apiIntegration =
        ApiIntegration' $
          apiIntegration
            { iId = applyValue apiIntegration.iId event.iId
            , name = applyValue apiIntegration.name event.name
            , props = applyValue apiIntegration.props event.props
            , logo = applyValue apiIntegration.logo event.logo
            , requestMethod = applyValue apiIntegration.requestMethod event.requestMethod
            , requestUrl = applyValue apiIntegration.requestUrl event.requestUrl
            , requestHeaders = applyValue apiIntegration.requestHeaders event.requestHeaders
            , requestBody = applyValue apiIntegration.requestBody event.requestBody
            , requestEmptySearch = applyValue apiIntegration.requestEmptySearch event.requestEmptySearch
            , responseListField = applyValue apiIntegration.responseListField event.responseListField
            , responseItemId = applyValue apiIntegration.responseItemId event.responseItemId
            , responseItemTemplate = applyValue apiIntegration.responseItemTemplate event.responseItemTemplate
            , itemUrl = applyValue apiIntegration.itemUrl event.itemUrl
            , annotations = applyValue apiIntegration.annotations event.annotations
            }
      applyToWidgetIntegration event widgetIntegration =
        WidgetIntegration' $
          widgetIntegration
            { iId = applyValue widgetIntegration.iId event.iId
            , name = applyValue widgetIntegration.name event.name
            , props = applyValue widgetIntegration.props event.props
            , logo = applyValue widgetIntegration.logo event.logo
            , widgetUrl = applyValue widgetIntegration.widgetUrl event.widgetUrl
            , itemUrl = applyValue widgetIntegration.itemUrl event.itemUrl
            , annotations = applyValue widgetIntegration.annotations event.annotations
            }

convertToApiIntegration :: Integration -> ApiIntegration
convertToApiIntegration (ApiIntegration' integration) = integration
convertToApiIntegration integration' =
  case integration' of
    (WidgetIntegration' integration) -> createIntegration integration
  where
    createIntegration integration =
      ApiIntegration
        { uuid = integration.uuid
        , iId = integration.iId
        , name = integration.name
        , props = integration.props
        , logo = integration.logo
        , requestMethod = ""
        , requestUrl = integration.widgetUrl
        , requestHeaders = []
        , requestBody = ""
        , requestEmptySearch = True
        , responseListField = ""
        , responseItemId = ""
        , responseItemTemplate = ""
        , itemUrl = integration.itemUrl
        , annotations = integration.annotations
        }

convertToWidgetIntegration :: Integration -> WidgetIntegration
convertToWidgetIntegration (WidgetIntegration' integration) = integration
convertToWidgetIntegration integration' =
  case integration' of
    (ApiIntegration' integration) -> createIntegration integration
  where
    createIntegration integration =
      WidgetIntegration
        { uuid = integration.uuid
        , iId = integration.iId
        , name = integration.name
        , props = integration.props
        , logo = integration.logo
        , widgetUrl = integration.requestUrl
        , itemUrl = integration.itemUrl
        , annotations = integration.annotations
        }
