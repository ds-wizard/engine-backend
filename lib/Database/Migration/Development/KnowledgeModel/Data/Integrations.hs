module Database.Migration.Development.KnowledgeModel.Data.Integrations where

import Control.Lens ((^.))
import Data.Map (fromList)
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

ontologyPortal :: Integration
ontologyPortal =
  Integration
  { _integrationUuid = fromJust $ U.fromString "e595a99e-5b10-4ac1-a6ef-379c849f9c84"
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
  }

ontologyPortalEdited :: Integration
ontologyPortalEdited =
  Integration
  { _integrationUuid = ontologyPortal ^. uuid
  , _integrationIId = "editedOntologyPortal"
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
  }

bioPortal :: Integration
bioPortal =
  Integration
  { _integrationUuid = fromJust $ U.fromString "32b5f11d-960b-4ce9-889f-fc7d29964122"
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
  }
