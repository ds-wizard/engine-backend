module Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration where

import qualified Data.Map.Strict as M

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddIntegrationEvent Integration where
  createEntity event (AddApiIntegrationEvent' content) =
    ApiIntegration' $
      ApiIntegration
        { uuid = event.entityUuid
        , name = content.name
        , variables = content.variables
        , allowCustomReply = content.allowCustomReply
        , requestMethod = content.requestMethod
        , requestUrl = content.requestUrl
        , requestHeaders = content.requestHeaders
        , requestBody = content.requestBody
        , requestAllowEmptySearch = content.requestAllowEmptySearch
        , responseListField = content.responseListField
        , responseItemTemplate = content.responseItemTemplate
        , responseItemTemplateForSelection = content.responseItemTemplateForSelection
        , testQ = content.testQ
        , testVariables = content.testVariables
        , testResponse = content.testResponse
        , annotations = content.annotations
        }
  createEntity event (AddApiLegacyIntegrationEvent' content) =
    ApiLegacyIntegration' $
      ApiLegacyIntegration
        { uuid = event.entityUuid
        , iId = content.iId
        , name = content.name
        , variables = content.variables
        , logo = content.logo
        , requestMethod = content.requestMethod
        , requestUrl = content.requestUrl
        , requestHeaders = content.requestHeaders
        , requestBody = content.requestBody
        , requestEmptySearch = content.requestEmptySearch
        , responseListField = content.responseListField
        , responseItemId = content.responseItemId
        , responseItemTemplate = content.responseItemTemplate
        , itemUrl = content.itemUrl
        , annotations = content.annotations
        }
  createEntity event (AddWidgetIntegrationEvent' content) =
    WidgetIntegration' $
      WidgetIntegration
        { uuid = event.entityUuid
        , iId = content.iId
        , name = content.name
        , variables = content.variables
        , logo = content.logo
        , widgetUrl = content.widgetUrl
        , itemUrl = content.itemUrl
        , annotations = content.annotations
        }

instance EditEntity EditIntegrationEvent Integration where
  editEntity event content' integration =
    case content' of
      (EditApiIntegrationEvent' content) -> applyToApiIntegration content . convertToApiIntegration $ integration
      (EditApiLegacyIntegrationEvent' content) -> applyToApiLegacyIntegration content . convertToApiLegacyIntegration $ integration
      (EditWidgetIntegrationEvent' content) -> applyToWidgetIntegration content . convertToWidgetIntegration $ integration
    where
      applyToApiIntegration :: EditApiIntegrationEvent -> ApiIntegration -> Integration
      applyToApiIntegration content apiIntegration =
        ApiIntegration' $
          apiIntegration
            { name = applyValue apiIntegration.name content.name
            , variables = applyValue apiIntegration.variables content.variables
            , allowCustomReply = applyValue apiIntegration.allowCustomReply content.allowCustomReply
            , requestMethod = applyValue apiIntegration.requestMethod content.requestMethod
            , requestUrl = applyValue apiIntegration.requestUrl content.requestUrl
            , requestHeaders = applyValue apiIntegration.requestHeaders content.requestHeaders
            , requestBody = applyValue apiIntegration.requestBody content.requestBody
            , requestAllowEmptySearch = applyValue apiIntegration.requestAllowEmptySearch content.requestAllowEmptySearch
            , responseListField = applyValue apiIntegration.responseListField content.responseListField
            , responseItemTemplate = applyValue apiIntegration.responseItemTemplate content.responseItemTemplate
            , responseItemTemplateForSelection = applyValue apiIntegration.responseItemTemplateForSelection content.responseItemTemplateForSelection
            , testQ = applyValue apiIntegration.testQ content.testQ
            , testVariables = applyValue apiIntegration.testVariables content.testVariables
            , testResponse = applyValue apiIntegration.testResponse content.testResponse
            , annotations = applyValue apiIntegration.annotations content.annotations
            }
      applyToApiLegacyIntegration content apiIntegration =
        ApiLegacyIntegration' $
          apiIntegration
            { iId = applyValue apiIntegration.iId content.iId
            , name = applyValue apiIntegration.name content.name
            , variables = applyValue apiIntegration.variables content.variables
            , logo = applyValue apiIntegration.logo content.logo
            , requestMethod = applyValue apiIntegration.requestMethod content.requestMethod
            , requestUrl = applyValue apiIntegration.requestUrl content.requestUrl
            , requestHeaders = applyValue apiIntegration.requestHeaders content.requestHeaders
            , requestBody = applyValue apiIntegration.requestBody content.requestBody
            , requestEmptySearch = applyValue apiIntegration.requestEmptySearch content.requestEmptySearch
            , responseListField = applyValue apiIntegration.responseListField content.responseListField
            , responseItemId = applyValue apiIntegration.responseItemId content.responseItemId
            , responseItemTemplate = applyValue apiIntegration.responseItemTemplate content.responseItemTemplate
            , itemUrl = applyValue apiIntegration.itemUrl content.itemUrl
            , annotations = applyValue apiIntegration.annotations content.annotations
            }
      applyToWidgetIntegration content widgetIntegration =
        WidgetIntegration' $
          widgetIntegration
            { iId = applyValue widgetIntegration.iId content.iId
            , name = applyValue widgetIntegration.name content.name
            , variables = applyValue widgetIntegration.variables content.variables
            , logo = applyValue widgetIntegration.logo content.logo
            , widgetUrl = applyValue widgetIntegration.widgetUrl content.widgetUrl
            , itemUrl = applyValue widgetIntegration.itemUrl content.itemUrl
            , annotations = applyValue widgetIntegration.annotations content.annotations
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
