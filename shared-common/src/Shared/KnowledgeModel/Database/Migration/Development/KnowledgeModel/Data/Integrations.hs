module Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations where

import qualified Data.Map.Strict as M

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.Aeson
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

repositoryApi' :: Integration
repositoryApi' = ApiIntegration' repositoryApi

repositoryApi :: ApiIntegration
repositoryApi =
  ApiIntegration
    { uuid = u' "40280ca6-aa18-4dee-9301-9b9c0800fef1"
    , name = "Repository API"
    , variables = ["domain", "country"]
    , allowCustomReply = True
    , requestMethod = "GET"
    , requestUrl = "{{secrets.repositoryApiUrl}}/{{secrets.repositoryPath}}?domain={{variables.domain}}&country={{variables.country}}&q={{q}}"
    , requestHeaders = [MapEntry "Api-Key" "{{secrets.repositoryApiKey}}"]
    , requestBody = Nothing
    , requestAllowEmptySearch = True
    , responseListField = Just "nested.results"
    , responseItemTemplate = "{{item.name}} ({{item.domain}})"
    , responseItemTemplateForSelection = Just "{{item.id}}: {{item.name}}"
    , testQ = "test"
    , testVariables = M.fromList [("domain", "biology"), ("country", "cz")]
    , testResponse = Just repositoryApiTypeHintExchange1
    , annotations = []
    }

repositoryApiTypeHintExchange1 :: TypeHintExchange
repositoryApiTypeHintExchange1 =
  TypeHintExchange
    { request = repositoryApiTypeHintRequest1
    , response = typeHintResponse1
    }

repositoryApiTypeHintRequest1 :: TypeHintRequest
repositoryApiTypeHintRequest1 =
  TypeHintRequest
    { method = "GET"
    , url = "[SECRET:repositoryApiUrl]/[SECRET:repositoryPath]?domain=biology&country=cz&q=biology"
    , headers = [MapEntry "Api-Key" "[SECRET:repositoryApiKey]"]
    , body = Nothing
    }

typeHintResponse1 :: TypeHintResponse
typeHintResponse1 = SuccessTypeHintResponse' successTypeHintResponse1

successTypeHintResponse1 :: SuccessTypeHintResponse
successTypeHintResponse1 =
  SuccessTypeHintResponse
    { status = 200
    , contentType = Just "application/json; charset=UTF-8"
    , body = "{\"nested\":{\"results\":[{\"id\":\"r000001\",\"name\":\"Forest Dataset\",\"domain\":\"biology\",\"country\":\"cz\"},{\"id\":\"r000002\",\"name\":\"Genomic Dataset\",\"domain\":\"biology\",\"country\":\"cz\"},{\"id\":\"r000003\",\"name\":\"Animals Dataset\",\"domain\":\"biology\",\"country\":\"cz\"}]}}\n"
    }

remoteErrorTypeHintResponse1 :: RemoteErrorTypeHintResponse
remoteErrorTypeHintResponse1 =
  RemoteErrorTypeHintResponse
    { status = 400
    , contentType = Just "application/json"
    , body = "{\"error\":\"Bad Request\"}"
    }

requestFailedTypeHintResponse1 :: RequestFailedTypeHintResponse
requestFailedTypeHintResponse1 =
  RequestFailedTypeHintResponse
    { message = "Request timed out after 30 seconds"
    }

repositoryApiEdited' :: Integration
repositoryApiEdited' = ApiIntegration' repositoryApiEdited

repositoryApiEdited :: ApiIntegration
repositoryApiEdited =
  ApiIntegration
    { uuid = repositoryApi.uuid
    , name = "Edited Repository API"
    , variables = ["domain", "country", "language"]
    , allowCustomReply = True
    , requestMethod = "GET"
    , requestUrl = "{{secrets.repositoryApiUrl}}/{{secrets.repositoryPath}}?domain={{variables.domain}}&country={{variables.country}}&language={{variables.language}}&q={{q}}"
    , requestHeaders = [MapEntry "Api-Key" "{{secrets.repositoryApiKey}}"]
    , requestBody = Nothing
    , requestAllowEmptySearch = True
    , responseListField = Just "nested.results"
    , responseItemTemplate = "{{item.name}} ({{item.domain}}, {{item.language}})"
    , responseItemTemplateForSelection = Just "{{item.id}}: {{item.name}}"
    , testQ = "test"
    , testVariables = M.fromList [("domain", "biology"), ("language", "en")]
    , testResponse = Just repositoryApiTypeHintExchange1
    , annotations = []
    }

orcidPluginIntegration' :: Integration
orcidPluginIntegration' = PluginIntegration' orcidPluginIntegration

orcidPluginIntegration :: PluginIntegration
orcidPluginIntegration =
  PluginIntegration
    { uuid = u' "dc19efbe-fdda-4f27-a51f-56662f4da808"
    , name = "ORCID"
    , pluginUuid = u' "d5524495-9a8d-4cee-a0b9-021df997fdc8"
    , pluginIntegrationId = "Orcid"
    , pluginIntegrationSettings = mapToObject (M.fromList [("setting1", "value1")])
    , annotations = []
    }

orcidPluginIntegrationEdited' :: Integration
orcidPluginIntegrationEdited' = PluginIntegration' orcidPluginIntegrationEdited

orcidPluginIntegrationEdited :: PluginIntegration
orcidPluginIntegrationEdited =
  PluginIntegration
    { uuid = u' "dc19efbe-fdda-4f27-a51f-56662f4da808"
    , name = "ORCID"
    , pluginUuid = u' "d5524495-9a8d-4cee-a0b9-021df997fdc8"
    , pluginIntegrationId = "EditedOrcid"
    , pluginIntegrationSettings = mapToObject (M.fromList [("setting1", "value1"), ("setting2", "value2")])
    , annotations = []
    }
