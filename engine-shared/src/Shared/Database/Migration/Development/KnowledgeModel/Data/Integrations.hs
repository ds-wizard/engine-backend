module Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations where

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

ontologyPortal' :: Integration
ontologyPortal' = ApiIntegration' ontologyPortal

ontologyPortal :: ApiIntegration
ontologyPortal =
  ApiIntegration
    { _apiIntegrationUuid = u' "e595a99e-5b10-4ac1-a6ef-379c849f9c84"
    , _apiIntegrationIId = "ontologyPortal"
    , _apiIntegrationName = "Ontology Portal"
    , _apiIntegrationProps = ["domain", "country"]
    , _apiIntegrationLogo = ""
    , _apiIntegrationRequestMethod = "GET"
    , _apiIntegrationRequestUrl = "${baseurl}/ontology-portal.json?domain=${domain}&country=${country}&q=${q}"
    , _apiIntegrationRequestHeaders = [MapEntry "Api-Key" "${apikey}"]
    , _apiIntegrationRequestBody = ""
    , _apiIntegrationRequestEmptySearch = True
    , _apiIntegrationResponseListField = "nested.results"
    , _apiIntegrationResponseItemId = "{{item.id}}"
    , _apiIntegrationResponseItemTemplate = "{{item.name}}"
    , _apiIntegrationItemUrl = "https://example.com/ontologies/${id}"
    , _apiIntegrationAnnotations = []
    }

ontologyPortalEdited' :: Integration
ontologyPortalEdited' = ApiIntegration' ontologyPortalEdited

ontologyPortalEdited :: ApiIntegration
ontologyPortalEdited =
  ontologyPortal
    { _apiIntegrationIId = "editedOntologyPortal"
    , _apiIntegrationName = "EDITED: Ontology Portal"
    , _apiIntegrationProps = ["domain", "language"]
    , _apiIntegrationLogo = ""
    , _apiIntegrationRequestMethod = "PUT"
    , _apiIntegrationRequestUrl =
        "${baseurl}/ontology-portal-edited.json?domain=${domain}&language=${language}&q=${q}&edited"
    , _apiIntegrationRequestHeaders = [MapEntry "Api-Key-Edited" "${apikey}-EDITED"]
    , _apiIntegrationRequestBody = "{}"
    , _apiIntegrationRequestEmptySearch = False
    , _apiIntegrationResponseListField = "nested.results"
    , _apiIntegrationResponseItemId = "EDITED: {{item.id}}"
    , _apiIntegrationResponseItemTemplate = "EDITED: {{item.name}}"
    , _apiIntegrationItemUrl = "https://example.com/ontologies-edited/{{item.id}}"
    , _apiIntegrationAnnotations = [MapEntry "newAnnotation" "someValue"]
    }

bioPortal' :: Integration
bioPortal' = ApiIntegration' bioPortal

bioPortal :: ApiIntegration
bioPortal =
  ApiIntegration
    { _apiIntegrationUuid = u' "32b5f11d-960b-4ce9-889f-fc7d29964122"
    , _apiIntegrationIId = "bioPortal"
    , _apiIntegrationName = "Bio Portal"
    , _apiIntegrationProps = ["domain", "branch"]
    , _apiIntegrationLogo = ""
    , _apiIntegrationRequestMethod = "GET"
    , _apiIntegrationRequestUrl = "${baseurl}/bio-portal.json?domain=${domain}&branch=${branch}&q=${q}"
    , _apiIntegrationRequestHeaders = [MapEntry "Api-Key" "${apikey}"]
    , _apiIntegrationRequestBody = ""
    , _apiIntegrationRequestEmptySearch = False
    , _apiIntegrationResponseListField = ""
    , _apiIntegrationResponseItemId = "{{item.id}}"
    , _apiIntegrationResponseItemTemplate = "{{item.name}}"
    , _apiIntegrationItemUrl = "https://example.com/portals/{{item.id}}"
    , _apiIntegrationAnnotations = []
    }

widgetPortal' :: Integration
widgetPortal' = WidgetIntegration' widgetPortal

widgetPortal :: WidgetIntegration
widgetPortal =
  WidgetIntegration
    { _widgetIntegrationUuid = u' "dc19efbe-fdda-4f27-a51f-56662f4da808"
    , _widgetIntegrationIId = "widgetPortal"
    , _widgetIntegrationName = "Widget Portal"
    , _widgetIntegrationProps = ["domain", "widgetType"]
    , _widgetIntegrationLogo = ""
    , _widgetIntegrationWidgetUrl = "${baseurl}/widget-portal.json?domain=${domain}&widgetType=${widgetType}&q=${q}"
    , _widgetIntegrationItemUrl = "https://example.com/widgets/{{item.id}}"
    , _widgetIntegrationAnnotations = []
    }

widgetPortalEdited' :: Integration
widgetPortalEdited' = WidgetIntegration' widgetPortalEdited

widgetPortalEdited :: WidgetIntegration
widgetPortalEdited =
  WidgetIntegration
    { _widgetIntegrationUuid = u' "dc19efbe-fdda-4f27-a51f-56662f4da808"
    , _widgetIntegrationIId = "editedBioPortal"
    , _widgetIntegrationName = "EDITED: Bio Portal"
    , _widgetIntegrationProps = ["domain", "branch"]
    , _widgetIntegrationLogo = ""
    , _widgetIntegrationWidgetUrl = "${baseurl}/bio-portal.json?domain=${domain}&branch=${branch}&q=${q}"
    , _widgetIntegrationItemUrl = "https://example.com/portals/{{item.id}}"
    , _widgetIntegrationAnnotations = []
    }
