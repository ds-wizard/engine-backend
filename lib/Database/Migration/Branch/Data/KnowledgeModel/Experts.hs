module Database.Migration.Branch.Data.KnowledgeModel.Experts where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

expertDarth :: Expert
expertDarth =
  Expert
  { _expertUuid = fromJust $ U.fromString "04b3661a-4176-4d5e-954f-6827f1888b8f"
  , _expertName = "Darth Vader"
  , _expertEmail = "darth.vader@deadstar.com"
  }

expertDarthChanged :: Expert
expertDarthChanged =
  Expert
  { _expertUuid = expertDarth ^. uuid
  , _expertName = "EDITED: Darth Vader"
  , _expertEmail = "EDITED: darth.vader@deadstar.com"
  }

expertLuke :: Expert
expertLuke =
  Expert
  { _expertUuid = fromJust $ U.fromString "bd078be4-fcfb-478a-b91b-45e0e57787b7"
  , _expertName = "Luke Skywalker"
  , _expertEmail = "luke.skywalker@tatooine.com"
  }

expertJohn :: Expert
expertJohn =
  Expert
  { _expertUuid = fromJust $ U.fromString "e56a5fea-6e01-4898-8db0-741200073752"
  , _expertName = "John Snow"
  , _expertEmail = "john.snow@got.com"
  }
