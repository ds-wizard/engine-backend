module Fixtures.KnowledgeModel.KnowledgeModels where

import Control.Lens

import Fixtures.KnowledgeModel.Chapters
import Model.KnowledgeModel.KnowledgeModel

km1 =
  KnowledgeModel
  { _kmUuid = "km1"
  , _kmName = "My Knowledge Model"
  , _kmChapters = [chapter1, chapter2]
  }

km1WithChangeProperties =
  KnowledgeModel
  { _kmUuid = "km1"
  , _kmName = "EDITED: My Knowledge Model"
  , _kmChapters = [chapter2, chapter1]
  }

km1WithoutChapters =
  KnowledgeModel
  { _kmUuid = "km1"
  , _kmName = "My Knowledge Model"
  , _kmChapters = []
  }

  