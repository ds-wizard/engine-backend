module Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration where

import qualified Data.Map.Strict as M

import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance CreateEntity AddIntegrationEvent Integration where
  createEntity (AddApiIntegrationEvent' event) =
    ApiIntegration' $
      ApiIntegration
        { uuid = event.entityUuid
        , name = event.name
        , variables = event.variables
        , allowCustomReply = event.allowCustomReply
        , requestMethod = event.requestMethod
        , requestUrl = event.requestUrl
        , requestHeaders = event.requestHeaders
        , requestBody = event.requestBody
        , requestAllowEmptySearch = event.requestAllowEmptySearch
        , responseListField = event.responseListField
        , responseItemTemplate = event.responseItemTemplate
        , responseItemTemplateForSelection = event.responseItemTemplateForSelection
        , testQ = event.testQ
        , testVariables = event.testVariables
        , testResponse = event.testResponse
        , annotations = event.annotations
        }
  createEntity (AddApiLegacyIntegrationEvent' event) =
    ApiLegacyIntegration' $
      ApiLegacyIntegration
        { uuid = event.entityUuid
        , iId = event.iId
        , name = event.name
        , variables = event.variables
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
        , variables = event.variables
        , logo = event.logo
        , widgetUrl = event.widgetUrl
        , itemUrl = event.itemUrl
        , annotations = event.annotations
        }

instance EditEntity EditIntegrationEvent Integration where
  editEntity event' integration =
    case event' of
      (EditApiIntegrationEvent' event) -> applyToApiIntegration event . convertToApiIntegration $ integration
      (EditApiLegacyIntegrationEvent' event) -> applyToApiLegacyIntegration event . convertToApiLegacyIntegration $ integration
      (EditWidgetIntegrationEvent' event) -> applyToWidgetIntegration event . convertToWidgetIntegration $ integration
    where
      applyToApiIntegration :: EditApiIntegrationEvent -> ApiIntegration -> Integration
      applyToApiIntegration event apiIntegration =
        ApiIntegration' $
          apiIntegration
            { name = applyValue apiIntegration.name event.name
            , variables = applyValue apiIntegration.variables event.variables
            , allowCustomReply = applyValue apiIntegration.allowCustomReply event.allowCustomReply
            , requestMethod = applyValue apiIntegration.requestMethod event.requestMethod
            , requestUrl = applyValue apiIntegration.requestUrl event.requestUrl
            , requestHeaders = applyValue apiIntegration.requestHeaders event.requestHeaders
            , requestBody = applyValue apiIntegration.requestBody event.requestBody
            , requestAllowEmptySearch = applyValue apiIntegration.requestAllowEmptySearch event.requestAllowEmptySearch
            , responseListField = applyValue apiIntegration.responseListField event.responseListField
            , responseItemTemplate = applyValue apiIntegration.responseItemTemplate event.responseItemTemplate
            , responseItemTemplateForSelection = applyValue apiIntegration.responseItemTemplateForSelection event.responseItemTemplateForSelection
            , testQ = applyValue apiIntegration.testQ event.testQ
            , testVariables = applyValue apiIntegration.testVariables event.testVariables
            , testResponse = applyValue apiIntegration.testResponse event.testResponse
            , annotations = applyValue apiIntegration.annotations event.annotations
            }
      applyToApiLegacyIntegration event apiIntegration =
        ApiLegacyIntegration' $
          apiIntegration
            { iId = applyValue apiIntegration.iId event.iId
            , name = applyValue apiIntegration.name event.name
            , variables = applyValue apiIntegration.variables event.variables
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
            , variables = applyValue widgetIntegration.variables event.variables
            , logo = applyValue widgetIntegration.logo event.logo
            , widgetUrl = applyValue widgetIntegration.widgetUrl event.widgetUrl
            , itemUrl = applyValue widgetIntegration.itemUrl event.itemUrl
            , annotations = applyValue widgetIntegration.annotations event.annotations
            }

convertToApiIntegration :: Integration -> ApiIntegration
convertToApiIntegration (ApiIntegration' integration) = integration
convertToApiIntegration (ApiLegacyIntegration' integration) =
  ApiIntegration
    { uuid = integration.uuid
    , name = integration.name
    , variables = integration.variables
    , allowCustomReply = True
    , requestMethod = integration.requestMethod
    , requestUrl = integration.requestUrl
    , requestHeaders = integration.requestHeaders
    , requestBody = Just integration.requestBody
    , requestAllowEmptySearch = integration.requestEmptySearch
    , responseListField = integration.responseListField
    , responseItemTemplate = integration.responseItemTemplate
    , responseItemTemplateForSelection = Nothing
    , testQ = ""
    , testVariables = M.empty
    , testResponse = Nothing
    , annotations = integration.annotations
    }
convertToApiIntegration (WidgetIntegration' integration) =
  ApiIntegration
    { uuid = integration.uuid
    , name = integration.name
    , variables = integration.variables
    , allowCustomReply = False
    , requestMethod = "GET"
    , requestUrl = integration.widgetUrl
    , requestHeaders = []
    , requestBody = Nothing
    , requestAllowEmptySearch = False
    , responseListField = Nothing
    , responseItemTemplate = ""
    , responseItemTemplateForSelection = Nothing
    , testQ = ""
    , testVariables = M.empty
    , testResponse = Nothing
    , annotations = integration.annotations
    }

convertToApiLegacyIntegration :: Integration -> ApiLegacyIntegration
convertToApiLegacyIntegration (ApiLegacyIntegration' integration) = integration
convertToApiLegacyIntegration (ApiIntegration' integration) =
  ApiLegacyIntegration
    { uuid = integration.uuid
    , iId = ""
    , name = integration.name
    , variables = integration.variables
    , logo = Nothing
    , requestMethod = integration.requestMethod
    , requestUrl = integration.requestUrl
    , requestHeaders = integration.requestHeaders
    , requestBody = ""
    , requestEmptySearch = True
    , responseListField = Nothing
    , responseItemId = Nothing
    , responseItemTemplate = ""
    , itemUrl = Nothing
    , annotations = integration.annotations
    }
convertToApiLegacyIntegration (WidgetIntegration' integration) =
  ApiLegacyIntegration
    { uuid = integration.uuid
    , iId = integration.iId
    , name = integration.name
    , variables = integration.variables
    , logo = integration.logo
    , requestMethod = ""
    , requestUrl = integration.widgetUrl
    , requestHeaders = []
    , requestBody = ""
    , requestEmptySearch = True
    , responseListField = Just ""
    , responseItemId = Just ""
    , responseItemTemplate = ""
    , itemUrl = integration.itemUrl
    , annotations = integration.annotations
    }

convertToWidgetIntegration :: Integration -> WidgetIntegration
convertToWidgetIntegration (WidgetIntegration' integration) = integration
convertToWidgetIntegration (ApiIntegration' integration) =
  WidgetIntegration
    { uuid = integration.uuid
    , iId = ""
    , name = integration.name
    , variables = integration.variables
    , logo = Nothing
    , widgetUrl = integration.requestUrl
    , itemUrl = Nothing
    , annotations = integration.annotations
    }
convertToWidgetIntegration (ApiLegacyIntegration' integration) =
  WidgetIntegration
    { uuid = integration.uuid
    , iId = integration.iId
    , name = integration.name
    , variables = integration.variables
    , logo = integration.logo
    , widgetUrl = integration.requestUrl
    , itemUrl = integration.itemUrl
    , annotations = integration.annotations
    }
