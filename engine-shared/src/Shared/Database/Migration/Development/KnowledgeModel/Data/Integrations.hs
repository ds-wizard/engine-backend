module Shared.Database.Migration.Development.KnowledgeModel.Data.Integrations where

import Data.Map (fromList)
import qualified Data.Map.Strict as M

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

ontologyPortal :: Integration
ontologyPortal =
  Integration
    { _integrationUuid = u' "e595a99e-5b10-4ac1-a6ef-379c849f9c84"
    , _integrationIId = "ontologyPortal"
    , _integrationName = "Ontology Portal"
    , _integrationProps = ["domain", "country"]
    , _integrationLogo = ""
    , _integrationRequestMethod = "GET"
    , _integrationRequestUrl = "${baseurl}/ontology-portal.json?domain=${domain}&country=${country}&q=${q}"
    , _integrationRequestHeaders = fromList [("Api-Key", "${apikey}")]
    , _integrationRequestBody = ""
    , _integrationResponseListField = "nested.results"
    , _integrationResponseIdField = "id"
    , _integrationResponseNameField = "name"
    , _integrationItemUrl = "https://example.com/ontologies/${id}"
    , _integrationAnnotations = M.empty
    }

ontologyPortalEdited :: Integration
ontologyPortalEdited =
  ontologyPortal
    { _integrationIId = "editedOntologyPortal"
    , _integrationName = "EDITED: Ontology Portal"
    , _integrationProps = ["domain", "language"]
    , _integrationLogo = ""
    , _integrationRequestMethod = "PUT"
    , _integrationRequestUrl =
        "${baseurl}/ontology-portal-edited.json?domain=${domain}&language=${language}&q=${q}&edited"
    , _integrationRequestHeaders = fromList [("Api-Key-Edited", "${apikey}-EDITED")]
    , _integrationRequestBody = "{}"
    , _integrationResponseListField = "nested.results"
    , _integrationResponseIdField = "idEdited"
    , _integrationResponseNameField = "nameEdited"
    , _integrationItemUrl = "https://example.com/ontologies-edited/${id}"
    , _integrationAnnotations = M.fromList [("newAnnotation", "someValue")]
    }

bioPortal :: Integration
bioPortal =
  Integration
    { _integrationUuid = u' "32b5f11d-960b-4ce9-889f-fc7d29964122"
    , _integrationIId = "bioPortal"
    , _integrationName = "Bio Portal"
    , _integrationProps = ["domain", "branch"]
    , _integrationLogo = ""
    , _integrationRequestMethod = "GET"
    , _integrationRequestUrl = "${baseurl}/bio-portal.json?domain=${domain}&branch=${branch}&q=${q}"
    , _integrationRequestHeaders = fromList [("Api-Key", "${apikey}")]
    , _integrationRequestBody = ""
    , _integrationResponseListField = ""
    , _integrationResponseIdField = "id"
    , _integrationResponseNameField = "name"
    , _integrationItemUrl = "https://example.com/portals/${id}"
    , _integrationAnnotations = M.empty
    }
