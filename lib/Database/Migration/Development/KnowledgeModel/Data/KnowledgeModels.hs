module Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.Tags
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

km1 :: KnowledgeModel
km1 =
  KnowledgeModel
  { _knowledgeModelUuid = fromJust $ U.fromString "ff672529-e837-4201-b7b1-7ada557d9725"
  , _knowledgeModelName = "Data Management Plan for Smart Researchers"
  , _knowledgeModelChapters = [chapter1, chapter2]
  , _knowledgeModelTags = [tagDataScience, tagBioInformatic]
  }

km1Edited :: KnowledgeModel
km1Edited =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = "EDITED: " ++ (km1 ^. name)
  , _knowledgeModelChapters = [chapter2, chapter1]
  , _knowledgeModelTags = [tagBioInformatic, tagDataScience]
  }

km1WithoutChaptersAndTags :: KnowledgeModel
km1WithoutChaptersAndTags =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = km1 ^. name
  , _knowledgeModelChapters = []
  , _knowledgeModelTags = []
  }

km1WithQ4Plain :: KnowledgeModel
km1WithQ4Plain =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = km1 ^. name
  , _knowledgeModelChapters = [chapter1, chapter2WithQ4Plain]
  , _knowledgeModelTags = [tagDataScience, tagBioInformatic]
  }

km1WithQ4 :: KnowledgeModel
km1WithQ4 =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = km1 ^. name
  , _knowledgeModelChapters = [chapter1, chapter2WithQ4]
  , _knowledgeModelTags = [tagDataScience, tagBioInformatic]
  }
