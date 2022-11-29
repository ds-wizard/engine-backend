module Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations where

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

ontologyPortal' :: Integration
ontologyPortal' = ApiIntegration' ontologyPortal

ontologyPortal :: ApiIntegration
ontologyPortal =
  ApiIntegration
    { uuid = u' "e595a99e-5b10-4ac1-a6ef-379c849f9c84"
    , iId = "ontologyPortal"
    , name = "Ontology Portal"
    , props = ["domain", "country"]
    , logo = ""
    , requestMethod = "GET"
    , requestUrl = "${baseurl}/${path}?domain=${domain}&country=${country}&q=${q}"
    , requestHeaders = [MapEntry "Api-Key" "${apikey}"]
    , requestBody = ""
    , requestEmptySearch = True
    , responseListField = "nested.results"
    , responseItemId = "{{item.id}}"
    , responseItemTemplate = "{{item.name}}"
    , itemUrl = "https://example.com/ontologies/${id}"
    , annotations = []
    }

ontologyPortalEdited' :: Integration
ontologyPortalEdited' = ApiIntegration' ontologyPortalEdited

ontologyPortalEdited :: ApiIntegration
ontologyPortalEdited =
  ontologyPortal
    { iId = "editedOntologyPortal"
    , name = "EDITED: Ontology Portal"
    , props = ["domain", "language"]
    , logo = ""
    , requestMethod = "PUT"
    , requestUrl = "${baseurl}/edited-${path}?domain=${domain}&language=${language}&q=${q}&edited"
    , requestHeaders = [MapEntry "Api-Key-Edited" "${apikey}-EDITED"]
    , requestBody = "{}"
    , requestEmptySearch = False
    , responseListField = "nested.results"
    , responseItemId = "EDITED: {{item.id}}"
    , responseItemTemplate = "EDITED: {{item.name}}"
    , itemUrl = "https://example.com/ontologies-edited/{{item.id}}"
    , annotations = [MapEntry "newAnnotation" "someValue"]
    }

bioPortal' :: Integration
bioPortal' = ApiIntegration' bioPortal

bioPortal :: ApiIntegration
bioPortal =
  ApiIntegration
    { uuid = u' "32b5f11d-960b-4ce9-889f-fc7d29964122"
    , iId = "bioPortal"
    , name = "Bio Portal"
    , props = ["domain", "branch"]
    , logo = ""
    , requestMethod = "GET"
    , requestUrl = "${baseurl}/${path}?domain=${domain}&branch=${branch}&q=${q}"
    , requestHeaders = [MapEntry "Api-Key" "${apikey}"]
    , requestBody = ""
    , requestEmptySearch = False
    , responseListField = ""
    , responseItemId = "{{item.id}}"
    , responseItemTemplate = "{{item.name}}"
    , itemUrl = "https://example.com/portals/{{item.id}}"
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
    , props = ["domain", "widgetType"]
    , logo = ""
    , widgetUrl = "${baseurl}/widget-portal.json?domain=${domain}&widgetType=${widgetType}&q=${q}"
    , itemUrl = "https://example.com/widgets/{{item.id}}"
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
    , props = ["domain", "branch"]
    , logo = ""
    , widgetUrl = "${baseurl}/bio-portal.json?domain=${domain}&branch=${branch}&q=${q}"
    , itemUrl = "https://example.com/portals/{{item.id}}"
    , annotations = []
    }
