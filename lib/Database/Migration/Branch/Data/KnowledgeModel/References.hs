module Database.Migration.Branch.Data.KnowledgeModel.References where

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

referenceCh1Changed' :: Reference
referenceCh1Changed' = ResourcePageReference' referenceCh1Changed

referenceCh1Changed :: ResourcePageReference
referenceCh1Changed =
  ResourcePageReference {_resourcePageReferenceUuid = referenceCh1 ^. uuid, _resourcePageReferenceShortUuid = "bbb"}

referenceCh2' :: Reference
referenceCh2' = URLReference' referenceCh2

referenceCh2 :: URLReference
referenceCh2 =
  URLReference
  { _uRLReferenceUuid = fromJust $ U.fromString "5004803d-43f6-4932-ab04-5a7e608894a5"
  , _uRLReferenceUrl = "http://dsw.com/fair"
  , _uRLReferenceAnchor = "F.A.I.R Principles"
  }

referenceCh3' :: Reference
referenceCh3' = CrossReference' referenceCh3

referenceCh3 :: CrossReference
referenceCh3 =
  CrossReference
  { _crossReferenceUuid = fromJust $ U.fromString "14255506-6c88-438d-a1ad-eea2071ee9cb"
  , _crossReferenceTargetUuid = fromJust $ U.fromString "2be1d749-9c72-4807-9309-d6c7bdbf13ba"
  }
