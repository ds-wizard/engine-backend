module Database.Migration.Development.FilledKnowledgeModel.Data.FilledChapters where

import Control.Lens ((^.))

import Database.Migration.Development.FilledKnowledgeModel.Data.FilledQuestions
import Database.Migration.Development.KnowledgeModel.Data.Chapters
import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

fChapter1 :: FilledChapter
fChapter1 =
  FilledChapter
  { _filledChapterUuid = chapter1 ^. uuid
  , _filledChapterHumanIdentifier = "I"
  , _filledChapterTitle = chapter1 ^. title
  , _filledChapterText = chapter1 ^. text
  , _filledChapterQuestions = [fQuestion1', fQuestion2']
  }

fChapter2 :: FilledChapter
fChapter2 =
  FilledChapter
  { _filledChapterUuid = chapter2 ^. uuid
  , _filledChapterHumanIdentifier = "II"
  , _filledChapterTitle = chapter2 ^. title
  , _filledChapterText = chapter2 ^. text
  , _filledChapterQuestions = [fQuestion3', fQuestion4']
  }

fChapter3 :: FilledChapter
fChapter3 =
  FilledChapter
  { _filledChapterUuid = chapter3 ^. uuid
  , _filledChapterHumanIdentifier = "III"
  , _filledChapterTitle = chapter3 ^. title
  , _filledChapterText = chapter3 ^. text
  , _filledChapterQuestions = [fQuestion9', fQuestion10']
  }
