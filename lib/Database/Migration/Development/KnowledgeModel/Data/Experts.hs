module Database.Migration.Development.KnowledgeModel.Data.Experts where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

expertAlbert :: Expert
expertAlbert =
  Expert
  { _expertUuid = fromJust $ U.fromString "04b3661a-4176-4d5e-954f-6827f1888b8f"
  , _expertName = "Albert Einstein"
  , _expertEmail = "albert.einstein@example.com"
  }

expertAlbertChanged :: Expert
expertAlbertChanged =
  Expert
  { _expertUuid = expertAlbert ^. uuid
  , _expertName = "EDITED: Albert Einstein"
  , _expertEmail = "EDITED: albert.einstein@example.com"
  }

expertNikola :: Expert
expertNikola =
  Expert
  { _expertUuid = fromJust $ U.fromString "bd078be4-fcfb-478a-b91b-45e0e57787b7"
  , _expertName = "Nikola Tesla"
  , _expertEmail = "nikola.tesla@example.com"
  }

expertIsaac :: Expert
expertIsaac =
  Expert
  { _expertUuid = fromJust $ U.fromString "e56a5fea-6e01-4898-8db0-741200073752"
  , _expertName = "Isaac Newton"
  , _expertEmail = "isaac.newton@example.com"
  }
