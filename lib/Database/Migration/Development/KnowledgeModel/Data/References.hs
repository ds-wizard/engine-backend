module Database.Migration.Development.KnowledgeModel.Data.References where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

referenceCh1' :: Reference
referenceCh1' = ResourcePageReference' referenceCh1

referenceCh1 :: ResourcePageReference
referenceCh1 =
  ResourcePageReference
  { _resourcePageReferenceUuid = fromJust $ U.fromString "a401b481-51b6-49ac-afca-ea957740e7ba"
  , _resourcePageReferenceShortUuid = "bvq"
  }

referenceCh1Edited' :: Reference
referenceCh1Edited' = ResourcePageReference' referenceCh1Edited

referenceCh1Edited :: ResourcePageReference
referenceCh1Edited =
  ResourcePageReference {_resourcePageReferenceUuid = referenceCh1 ^. uuid, _resourcePageReferenceShortUuid = "bbb"}

referenceCh1WithNewType' :: Reference
referenceCh1WithNewType' = URLReference' referenceCh1WithNewType

referenceCh1WithNewType :: URLReference
referenceCh1WithNewType =
  URLReference
  { _uRLReferenceUuid = referenceCh1 ^. uuid
  , _uRLReferenceUrl = "https://ds-wizard.org/dmp"
  , _uRLReferenceLabel = "DMP Guide"
  }

-- ---------------------------------------------------------------------------
referenceCh2' :: Reference
referenceCh2' = URLReference' referenceCh2

referenceCh2 :: URLReference
referenceCh2 =
  URLReference
  { _uRLReferenceUuid = fromJust $ U.fromString "5004803d-43f6-4932-ab04-5a7e608894a5"
  , _uRLReferenceUrl = "https://ds-wizard.org/fair"
  , _uRLReferenceLabel = "F.A.I.R Principles"
  }

referenceCh2Edited' :: Reference
referenceCh2Edited' = URLReference' referenceCh2Edited

referenceCh2Edited :: URLReference
referenceCh2Edited =
  URLReference
  { _uRLReferenceUuid = referenceCh2 ^. uuid
  , _uRLReferenceUrl = "EDITED: " ++ referenceCh2 ^. url
  , _uRLReferenceLabel = "EDITED: " ++ referenceCh2 ^. label
  }

referenceCh2WithNewType' :: Reference
referenceCh2WithNewType' = CrossReference' referenceCh2WithNewType

referenceCh2WithNewType :: CrossReference
referenceCh2WithNewType =
  CrossReference
  { _crossReferenceUuid = referenceCh2 ^. uuid
  , _crossReferenceTargetUuid = fromJust $ U.fromString "1b20169a-4a37-41d1-875c-f0ca507dc438"
  , _crossReferenceDescription = "Link to my target"
  }

-- ---------------------------------------------------------------------------
referenceCh3' :: Reference
referenceCh3' = CrossReference' referenceCh3

referenceCh3 :: CrossReference
referenceCh3 =
  CrossReference
  { _crossReferenceUuid = fromJust $ U.fromString "14255506-6c88-438d-a1ad-eea2071ee9cb"
  , _crossReferenceTargetUuid = fromJust $ U.fromString "2be1d749-9c72-4807-9309-d6c7bdbf13ba"
  , _crossReferenceDescription = "Some description"
  }

referenceCh3Edited' :: Reference
referenceCh3Edited' = CrossReference' referenceCh3Edited

referenceCh3Edited :: CrossReference
referenceCh3Edited =
  CrossReference
  { _crossReferenceUuid = referenceCh3 ^. uuid
  , _crossReferenceTargetUuid = fromJust $ U.fromString "7c24a6f4-72ba-4744-94c4-30f699d6edbf"
  , _crossReferenceDescription = "EDITED: " ++ referenceCh3 ^. description
  }

referenceCh3WithNewType' :: Reference
referenceCh3WithNewType' = ResourcePageReference' referenceCh3WithNewType

referenceCh3WithNewType :: ResourcePageReference
referenceCh3WithNewType =
  ResourcePageReference {_resourcePageReferenceUuid = referenceCh3 ^. uuid, _resourcePageReferenceShortUuid = "awp"}
