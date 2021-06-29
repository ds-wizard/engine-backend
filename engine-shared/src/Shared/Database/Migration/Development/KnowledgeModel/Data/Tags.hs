module Shared.Database.Migration.Development.KnowledgeModel.Data.Tags where

import Control.Lens ((^.))
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel

tagDataScience :: Tag
tagDataScience =
  Tag
    { _tagUuid = fromJust $ U.fromString "b2f3c232-018b-4d70-8e90-b5c81e8006f1"
    , _tagName = "Data Science"
    , _tagDescription = Just "Questions related to data science"
    , _tagColor = "#4A90E2"
    }

tagDataScienceEdited :: Tag
tagDataScienceEdited =
  Tag
    { _tagUuid = tagDataScience ^. uuid
    , _tagName = "EDITED: " ++ (tagDataScience ^. name)
    , _tagDescription = Just $ "EDITED: " ++ fromJust (tagDataScience ^. description)
    , _tagColor = "EDITED: " ++ (tagDataScience ^. color)
    }

tagBioInformatic :: Tag
tagBioInformatic =
  Tag
    { _tagUuid = fromJust $ U.fromString "e58abfb7-479d-4e81-95e0-83654e83da1a"
    , _tagName = "BioInformatic"
    , _tagDescription = Just "Questions related to bio informatic engineering"
    , _tagColor = "#F5A623"
    }
