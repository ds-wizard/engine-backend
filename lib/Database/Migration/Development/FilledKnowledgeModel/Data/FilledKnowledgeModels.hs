module Database.Migration.Development.FilledKnowledgeModel.Data.FilledKnowledgeModels where

import Control.Lens ((^.))

import Database.Migration.Development.FilledKnowledgeModel.Data.FilledChapters
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

fKm1WithQ4 :: FilledKnowledgeModel
fKm1WithQ4 =
  FilledKnowledgeModel
  { _filledKnowledgeModelUuid = km1 ^. uuid
  , _filledKnowledgeModelName = km1 ^. name
  , _filledKnowledgeModelChapters = [fChapter1, fChapter2, fChapter3]
  , _filledKnowledgeModelTags = km1 ^. tags
  , _filledKnowledgeModelIntegrations = km1 ^. integrations
  }
