module WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations where

import qualified Data.Map.Strict as M

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.Uuid
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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

ontologyPortal' :: Integration
ontologyPortal' = ApiLegacyIntegration' ontologyPortal

ontologyPortal :: ApiLegacyIntegration
ontologyPortal =
  ApiLegacyIntegration
    { uuid = u' "e595a99e-5b10-4ac1-a6ef-379c849f9c84"
    , iId = "ontologyPortal"
    , name = "Ontology Portal"
    , variables = ["domain", "country"]
    , logo = Nothing
    , requestMethod = "GET"
    , requestUrl = "${baseurl}/${path}?domain=${domain}&country=${country}&q=${q}"
    , requestHeaders = [MapEntry "Api-Key" "${apiKey}"]
    , requestBody = ""
    , requestEmptySearch = True
    , responseListField = Just "nested.results"
    , responseItemId = Just "{{item.id}}"
    , responseItemTemplate = "{{item.name}}"
    , itemUrl = Just "https://example.com/ontologies/${id}"
    , annotations = []
    }

ontologyPortalEdited' :: Integration
ontologyPortalEdited' = ApiLegacyIntegration' ontologyPortalEdited

ontologyPortalEdited :: ApiLegacyIntegration
ontologyPortalEdited =
  ontologyPortal
    { iId = "editedOntologyPortal"
    , name = "EDITED: Ontology Portal"
    , variables = ["domain", "language"]
    , logo = Nothing
    , requestMethod = "PUT"
    , requestUrl = "${baseurl}/edited-${path}?domain=${domain}&language=${language}&q=${q}&edited"
    , requestHeaders = [MapEntry "Api-Key-Edited" "${apiKey}-EDITED"]
    , requestBody = "{}"
    , requestEmptySearch = False
    , responseListField = Just "nested.results"
    , responseItemId = Just "EDITED: {{item.id}}"
    , responseItemTemplate = "EDITED: {{item.name}}"
    , itemUrl = Just "https://example.com/ontologies-edited/{{item.id}}"
    , annotations = [MapEntry "newAnnotation" "someValue"]
    }

bioPortal' :: Integration
bioPortal' = ApiLegacyIntegration' bioPortal

bioPortal :: ApiLegacyIntegration
bioPortal =
  ApiLegacyIntegration
    { uuid = u' "32b5f11d-960b-4ce9-889f-fc7d29964122"
    , iId = "bioPortal"
    , name = "Bio Portal"
    , variables = ["domain", "branch"]
    , logo = Nothing
    , requestMethod = "GET"
    , requestUrl = "${baseurl}/${path}?domain=${domain}&branch=${branch}&q=${q}"
    , requestHeaders = [MapEntry "Api-Key" "${apiKey}"]
    , requestBody = ""
    , requestEmptySearch = False
    , responseListField = Nothing
    , responseItemId = Just "{{item.id}}"
    , responseItemTemplate = "{{item.name}}"
    , itemUrl = Just "https://example.com/portals/{{item.id}}"
    , annotations = []
    }

widgetPortal' :: Integration
widgetPortal' = WidgetIntegration' widgetPortal

widgetPortal :: WidgetIntegration
widgetPortal =
  WidgetIntegration
    { uuid = u' "dc19efbe-fdda-4f27-a51f-56662f4da808"
    , iId = "widgetPortal"
    , name = "Widget Portal"
    , variables = ["domain", "widgetType"]
    , logo = Nothing
    , widgetUrl = "${baseurl}/widget-portal.json?domain=${domain}&widgetType=${widgetType}&q=${q}"
    , itemUrl = Just "https://example.com/widgets/{{item.id}}"
    , annotations = []
    }

widgetPortalEdited' :: Integration
widgetPortalEdited' = WidgetIntegration' widgetPortalEdited

widgetPortalEdited :: WidgetIntegration
widgetPortalEdited =
  WidgetIntegration
    { uuid = u' "dc19efbe-fdda-4f27-a51f-56662f4da808"
    , iId = "editedBioPortal"
    , name = "EDITED: Bio Portal"
    , variables = ["domain", "branch"]
    , logo = Nothing
    , widgetUrl = "${baseurl}/bio-portal.json?domain=${domain}&branch=${branch}&q=${q}"
    , itemUrl = Just "https://example.com/portals/{{item.id}}"
    , annotations = []
    }
