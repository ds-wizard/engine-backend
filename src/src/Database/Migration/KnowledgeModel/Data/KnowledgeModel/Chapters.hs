module Database.Migration.KnowledgeModel.Data.KnowledgeModel.Chapters where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.KnowledgeModel.Data.KnowledgeModel.Questions
       as FQ
import Model.KnowledgeModel.KnowledgeModel

chapter1 :: Chapter
chapter1 =
  Chapter
  { _chUuid = fromJust $ U.fromString "e3c4b314-919b-418d-bb85-a07c97433427"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Design of experiment"
  , _chText = "Looong long text"
  , _chQuestions = [FQ.question1, FQ.question2]
  }

-- chapter1WithChangedTitle = chapter1 & chTitle .~ "EDITED: Design of experiment"
-- chapter1WithChangedText = chapter1 & chText .~ "EDITED: Looong long text"
chapter1WithChangeProperties :: Chapter
chapter1WithChangeProperties =
  Chapter
  { _chUuid = chapter1 ^. chUuid
  , _chNamespace = chapter1 ^. chNamespace
  , _chFormatVersion = chapter1 ^. chFormatVersion
  , _chTitle = "EDITED: Design of experiment"
  , _chText = "EDITED: Looong long text"
  , _chQuestions = [FQ.question2, FQ.question1]
  }

chapter1WithChangedQuestion2 :: Chapter
chapter1WithChangedQuestion2 =
  Chapter
  { _chUuid = chapter1 ^. chUuid
  , _chNamespace = chapter1 ^. chNamespace
  , _chFormatVersion = chapter1 ^. chFormatVersion
  , _chTitle = chapter1 ^. chTitle
  , _chText = chapter1 ^. chText
  , _chQuestions = [FQ.question1, FQ.question2WithChangeProperties]
  }

chapter1WithoutQuestions :: Chapter
chapter1WithoutQuestions =
  Chapter
  { _chUuid = chapter1 ^. chUuid
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Design of experiment"
  , _chText = "Looong long text"
  , _chQuestions = []
  }

chapter1WithAddedQuestion3 :: Chapter
chapter1WithAddedQuestion3 =
  Chapter
  { _chUuid = chapter1 ^. chUuid
  , _chNamespace = chapter1 ^. chNamespace
  , _chFormatVersion = chapter1 ^. chFormatVersion
  , _chTitle = chapter1 ^. chTitle
  , _chText = chapter1 ^. chText
  , _chQuestions = chapter1 ^. chQuestions ++ [FQ.question3Plain]
  }

chapter2 :: Chapter
chapter2 =
  Chapter
  { _chUuid = fromJust $ U.fromString "a2a25254-58e3-4fc7-9850-160803d792fe"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Implementation"
  , _chText = "Some long text"
  , _chQuestions = [FQ.question3]
  }

chapter2WithoutQuestions :: Chapter
chapter2WithoutQuestions =
  Chapter
  { _chUuid = chapter2 ^. chUuid
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Implementation"
  , _chText = "Some long text"
  , _chQuestions = []
  }

chapter3WithoutQuestions :: Chapter
chapter3WithoutQuestions =
  Chapter
  { _chUuid = fromJust $ U.fromString "ca46cd40-3999-4f0d-a8ba-15d57682dfeb"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Testing"
  , _chText = "Some long text"
  , _chQuestions = []
  }

createChapter :: [Question] -> Chapter
createChapter qs =
  Chapter
  { _chUuid = fromJust $ U.fromString "a2a25254-58e3-4fc7-9850-160803d792fe"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Design of experiment"
  , _chText = "Looong long text"
  , _chQuestions = qs
  }
