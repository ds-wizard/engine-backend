module Database.Migration.KnowledgeModel.Data.KnowledgeModel.Experts where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Model.KnowledgeModel.KnowledgeModel

expertDarth :: Expert
expertDarth =
  Expert
  { _expUuid = fromJust $ U.fromString "04b3661a-4176-4d5e-954f-6827f1888b8f"
  , _expName = "Darth Vader"
  , _expEmail = "darth.vader@deadstar.com"
  }

expertDarthChanged :: Expert
expertDarthChanged =
  Expert
  { _expUuid = expertDarth ^. expUuid
  , _expName = "EDITED: Darth Vader"
  , _expEmail = "EDITED: darth.vader@deadstar.com"
  }

expertLuke :: Expert
expertLuke =
  Expert
  { _expUuid = fromJust $ U.fromString "bd078be4-fcfb-478a-b91b-45e0e57787b7"
  , _expName = "Luke Skywalker"
  , _expEmail = "luke.skywalker@tatooine.com"
  }

expertJohn :: Expert
expertJohn =
  Expert
  { _expUuid = fromJust $ U.fromString "e56a5fea-6e01-4898-8db0-741200073752"
  , _expName = "John Snow"
  , _expEmail = "john.snow@got.com"
  }
