module Shared.Database.Migration.Development.KnowledgeModel.Data.Tags where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

tagDataScience :: Tag
tagDataScience =
  Tag
    { _tagUuid = u' "b2f3c232-018b-4d70-8e90-b5c81e8006f1"
    , _tagName = "Data Science"
    , _tagDescription = Just "Questions related to data science"
    , _tagColor = "#4A90E2"
    , _tagAnnotations = []
    }

tagDataScienceEdited :: Tag
tagDataScienceEdited =
  Tag
    { _tagUuid = tagDataScience ^. uuid
    , _tagName = "EDITED: " ++ (tagDataScience ^. name)
    , _tagDescription = fmap ("EDITED: " ++) (tagDataScience ^. description)
    , _tagColor = "EDITED: " ++ (tagDataScience ^. color)
    , _tagAnnotations = [MapEntry "newAnnotation" "someValue"]
    }

tagBioInformatic :: Tag
tagBioInformatic =
  Tag
    { _tagUuid = u' "e58abfb7-479d-4e81-95e0-83654e83da1a"
    , _tagName = "BioInformatic"
    , _tagDescription = Just "Questions related to bio informatic engineering"
    , _tagColor = "#F5A623"
    , _tagAnnotations = []
    }
