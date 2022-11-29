module Shared.Database.Migration.Development.KnowledgeModel.Data.Tags where

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

tagDataScience :: Tag
tagDataScience =
  Tag
    { uuid = u' "b2f3c232-018b-4d70-8e90-b5c81e8006f1"
    , name = "Data Science"
    , description = Just "Questions related to data science"
    , color = "#4A90E2"
    , annotations = []
    }

tagDataScienceEdited :: Tag
tagDataScienceEdited =
  Tag
    { uuid = tagDataScience.uuid
    , name = "EDITED: " ++ tagDataScience.name
    , description = fmap ("EDITED: " ++) tagDataScience.description
    , color = "EDITED: " ++ tagDataScience.color
    , annotations = [MapEntry "newAnnotation" "someValue"]
    }

tagBioInformatic :: Tag
tagBioInformatic =
  Tag
    { uuid = u' "e58abfb7-479d-4e81-95e0-83654e83da1a"
    , name = "BioInformatic"
    , description = Just "Questions related to bio informatic engineering"
    , color = "#F5A623"
    , annotations = []
    }
