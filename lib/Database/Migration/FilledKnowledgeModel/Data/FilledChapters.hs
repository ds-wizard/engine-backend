module Database.Migration.FilledKnowledgeModel.Data.FilledChapters where

import Control.Lens ((^.))

import Database.Migration.Branch.Data.KnowledgeModel.Chapters
import Database.Migration.FilledKnowledgeModel.Data.FilledQuestions
import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

fChapter1 =
  FilledChapter
  { _filledChapterUuid = chapter1 ^. uuid
  , _filledChapterTitle = chapter1 ^. title
  , _filledChapterText = chapter1 ^. text
  , _filledChapterQuestions = [fQuestion1, fQuestion2]
  }

fChapter2 =
  FilledChapter
  { _filledChapterUuid = chapter2 ^. uuid
  , _filledChapterTitle = chapter2 ^. title
  , _filledChapterText = chapter2 ^. text
  , _filledChapterQuestions = [fQuestion3, fQuestion4]
  }
