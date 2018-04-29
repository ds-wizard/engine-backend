module Database.Migration.Branch.Data.KnowledgeModel.Chapters where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Branch.Data.KnowledgeModel.Questions
       as FQ
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

chapter1 :: Chapter
chapter1 =
  Chapter
  { _chapterUuid = fromJust $ U.fromString "e3c4b314-919b-418d-bb85-a07c97433427"
  , _chapterTitle = "Design of experiment"
  , _chapterText = "Looong long text"
  , _chapterQuestions = [FQ.question1, FQ.question2]
  }

chapter1WithChangeProperties :: Chapter
chapter1WithChangeProperties =
  Chapter
  { _chapterUuid = chapter1 ^. uuid
  , _chapterTitle = "EDITED: Design of experiment"
  , _chapterText = "EDITED: Looong long text"
  , _chapterQuestions = [FQ.question2, FQ.question1]
  }

chapter1WithChangedQuestion2 :: Chapter
chapter1WithChangedQuestion2 =
  Chapter
  { _chapterUuid = chapter1 ^. uuid
  , _chapterTitle = chapter1 ^. title
  , _chapterText = chapter1 ^. text
  , _chapterQuestions = [FQ.question1, FQ.question2WithChangeProperties]
  }

chapter1WithoutQuestions :: Chapter
chapter1WithoutQuestions =
  Chapter
  { _chapterUuid = chapter1 ^. uuid
  , _chapterTitle = "Design of experiment"
  , _chapterText = "Looong long text"
  , _chapterQuestions = []
  }

chapter1WithAddedQuestion3 :: Chapter
chapter1WithAddedQuestion3 =
  Chapter
  { _chapterUuid = chapter1 ^. uuid
  , _chapterTitle = chapter1 ^. title
  , _chapterText = chapter1 ^. text
  , _chapterQuestions = chapter1 ^. questions ++ [FQ.question3Plain]
  }

chapter2 :: Chapter
chapter2 =
  Chapter
  { _chapterUuid = fromJust $ U.fromString "a2a25254-58e3-4fc7-9850-160803d792fe"
  , _chapterTitle = "Implementation"
  , _chapterText = "Some long text"
  , _chapterQuestions = [FQ.question3]
  }

chapter2WithoutQuestions :: Chapter
chapter2WithoutQuestions =
  Chapter
  { _chapterUuid = chapter2 ^. uuid
  , _chapterTitle = chapter2 ^. title
  , _chapterText = chapter2 ^. text
  , _chapterQuestions = []
  }

chapter2WithQ4Plain :: Chapter
chapter2WithQ4Plain =
  Chapter
  { _chapterUuid = chapter2 ^. uuid
  , _chapterTitle = chapter2 ^. title
  , _chapterText = chapter2 ^. text
  , _chapterQuestions = [FQ.question3, FQ.question4Plain]
  }

chapter2WithQ4 :: Chapter
chapter2WithQ4 =
  Chapter
  { _chapterUuid = chapter2 ^. uuid
  , _chapterTitle = chapter2 ^. title
  , _chapterText = chapter2 ^. text
  , _chapterQuestions = [FQ.question3, FQ.question4]
  }

chapter3WithoutQuestions :: Chapter
chapter3WithoutQuestions =
  Chapter
  { _chapterUuid = fromJust $ U.fromString "ca46cd40-3999-4f0d-a8ba-15d57682dfeb"
  , _chapterTitle = "Testing"
  , _chapterText = "Some long text"
  , _chapterQuestions = []
  }

createChapter :: [Question] -> Chapter
createChapter qs =
  Chapter
  { _chapterUuid = fromJust $ U.fromString "a2a25254-58e3-4fc7-9850-160803d792fe"
  , _chapterTitle = "Design of experiment"
  , _chapterText = "Looong long text"
  , _chapterQuestions = qs
  }
