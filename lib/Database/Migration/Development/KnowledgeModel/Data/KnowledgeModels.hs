module Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.Integrations
import Database.Migration.Development.KnowledgeModel.Data.Tags
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

km1 :: KnowledgeModel
km1 =
  KnowledgeModel
  { _knowledgeModelUuid = fromJust $ U.fromString "ff672529-e837-4201-b7b1-7ada557d9725"
  , _knowledgeModelName = "Data Management Plan for Smart Researchers"
  , _knowledgeModelChapters = [chapter1, chapter2, chapter3]
  , _knowledgeModelTags = [tagDataScience, tagBioInformatic]
  , _knowledgeModelIntegrations = [ontologyPortal, bioPortal]
  }

km1Edited :: KnowledgeModel
km1Edited =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = "EDITED: " ++ (km1 ^. name)
  , _knowledgeModelChapters = [chapter3, chapter2, chapter1]
  , _knowledgeModelTags = [tagBioInformatic, tagDataScience]
  , _knowledgeModelIntegrations = [bioPortal, ontologyPortal]
  }

km1WithoutChaptersAndTagsAndIntegrations :: KnowledgeModel
km1WithoutChaptersAndTagsAndIntegrations =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = km1 ^. name
  , _knowledgeModelChapters = []
  , _knowledgeModelTags = []
  , _knowledgeModelIntegrations = []
  }

km1WithQ4Plain :: KnowledgeModel
km1WithQ4Plain =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = km1 ^. name
  , _knowledgeModelChapters = [chapter1, chapter2WithQ4Plain, chapter3]
  , _knowledgeModelTags = [tagDataScience, tagBioInformatic]
  , _knowledgeModelIntegrations = [ontologyPortal, bioPortal]
  }

km1WithQ4 :: KnowledgeModel
km1WithQ4 =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = km1 ^. name
  , _knowledgeModelChapters = [chapter1, chapter2WithQ4, chapter3]
  , _knowledgeModelTags = [tagDataScience, tagBioInformatic]
  , _knowledgeModelIntegrations = [ontologyPortal, bioPortal]
  }

km1Netherlands :: KnowledgeModel
km1Netherlands =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = km1 ^. name
  , _knowledgeModelChapters = [chapter1WithoutQuestions]
  , _knowledgeModelTags = [tagDataScience, tagBioInformatic]
  , _knowledgeModelIntegrations = [ontologyPortal, bioPortal]
  }

km1NetherlandsV2 :: KnowledgeModel
km1NetherlandsV2 =
  KnowledgeModel
  { _knowledgeModelUuid = km1 ^. uuid
  , _knowledgeModelName = km1 ^. name
  , _knowledgeModelChapters = [chapter1WithoutQuestions, chapter4WithoutQuestions]
  , _knowledgeModelTags = [tagDataScience, tagBioInformatic]
  , _knowledgeModelIntegrations = [ontologyPortal, bioPortal]
  }
