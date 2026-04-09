module Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Util.Aeson
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
  createEntity event (AddPluginIntegrationEvent' content) =
    PluginIntegration' $
      PluginIntegration
        { uuid = event.entityUuid
        , name = content.name
        , pluginUuid = content.pluginUuid
        , pluginIntegrationId = content.pluginIntegrationId
        , pluginIntegrationSettings = content.pluginIntegrationSettings
        , annotations = content.annotations
        }

instance EditEntity EditIntegrationEvent Integration where
  editEntity event content' integration =
    case content' of
      (EditApiIntegrationEvent' content) -> applyToApiIntegration content . convertToApiIntegration $ integration
      (EditPluginIntegrationEvent' content) -> applyToPluginIntegration content . convertToPluginIntegration $ integration
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
      applyToPluginIntegration content pluginIntegration =
        PluginIntegration' $
          pluginIntegration
            { name = applyValue pluginIntegration.name content.name
            , pluginUuid = applyValue pluginIntegration.pluginUuid content.pluginUuid
            , pluginIntegrationId = applyValue pluginIntegration.pluginIntegrationId content.pluginIntegrationId
            , pluginIntegrationSettings = applyValue pluginIntegration.pluginIntegrationSettings content.pluginIntegrationSettings
            , annotations = applyValue pluginIntegration.annotations content.annotations
            }

convertToApiIntegration :: Integration -> ApiIntegration
convertToApiIntegration (ApiIntegration' integration) = integration
convertToApiIntegration (PluginIntegration' integration) =
  ApiIntegration
    { uuid = integration.uuid
    , name = ""
    , variables = []
    , allowCustomReply = False
    , requestMethod = "GET"
    , requestUrl = ""
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

convertToPluginIntegration :: Integration -> PluginIntegration
convertToPluginIntegration (PluginIntegration' integration) = integration
convertToPluginIntegration (ApiIntegration' integration) =
  PluginIntegration
    { uuid = integration.uuid
    , name = integration.name
    , pluginUuid = U.nil
    , pluginIntegrationId = ""
    , pluginIntegrationSettings = mapToObject M.empty
    , annotations = integration.annotations
    }
