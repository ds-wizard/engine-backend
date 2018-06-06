module Database.Migration.FilledKnowledgeModel.Data.FilledKnowledgeModels where

import Control.Lens ((^.))

import Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
import Database.Migration.FilledKnowledgeModel.Data.FilledChapters
import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

fKm1WithQ4 =
  FilledKnowledgeModel
  { _filledKnowledgeModelUuid = km1 ^. uuid
  , _filledKnowledgeModelName = km1 ^. name
  , _filledKnowledgeModelChapters = [fChapter1, fChapter2]
  }
