module Fixtures.KnowledgeModel.Chapters where

import Control.Lens

import Fixtures.KnowledgeModel.Questions as FQ
import Model.KnowledgeModel.KnowledgeModel

chapter1 =
  Chapter
  { _chUuid = "chapter1"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Design of experiment"
  , _chText = "Looong long text"
  , _chQuestions = [FQ.question1, FQ.question2]
  }

-- chapter1WithChangedTitle = chapter1 & chTitle .~ "EDITED: Design of experiment"

-- chapter1WithChangedText = chapter1 & chText .~ "EDITED: Looong long text"

chapter1WithChangeProperties =
  Chapter
  { _chUuid = chapter1 ^. chUuid
  , _chNamespace = chapter1 ^. chNamespace
  , _chFormatVersion = chapter1 ^. chFormatVersion
  , _chTitle = "EDITED: Design of experiment"
  , _chText = "EDITED: Looong long text"
  , _chQuestions = [FQ.question2, FQ.question1]
  }

chapter1WithChangedQuestion2 = 
  Chapter
  { _chUuid = chapter1 ^. chUuid
  , _chNamespace = chapter1 ^. chNamespace
  , _chFormatVersion = chapter1 ^. chFormatVersion
  , _chTitle = chapter1 ^. chTitle
  , _chText = chapter1 ^. chText
  , _chQuestions = [FQ.question1, FQ.question2WithChangeProperties]
  }

chapter1WithoutQuestions =
  Chapter
  { _chUuid = "chapter1"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Design of experiment"
  , _chText = "Looong long text"
  , _chQuestions = []
  }
  
chapter1WithAddedQuestion3 = 
  Chapter
  { _chUuid = chapter1 ^. chUuid
  , _chNamespace = chapter1 ^. chNamespace
  , _chFormatVersion = chapter1 ^. chFormatVersion
  , _chTitle = chapter1 ^. chTitle
  , _chText = chapter1 ^. chText
  , _chQuestions = chapter1 ^. chQuestions ++ [FQ.question3Plain]
  }

chapter2 =
  Chapter
  { _chUuid = "chapter2"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Implementation"
  , _chText = "Some long text"
  , _chQuestions = [FQ.question3]
  }

chapter2WithoutQuestions =
  Chapter
  { _chUuid = "chapter2"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Implementation"
  , _chText = "Some long text"
  , _chQuestions = []
  }

chapter3WithoutQuestions =
  Chapter
  { _chUuid = "chapter3"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Testing"
  , _chText = "Some long text"
  , _chQuestions = []
  }
  

createChapter :: [Question] -> Chapter
createChapter qs =
  Chapter
  { _chUuid = "chapter2"
  , _chNamespace = "core"
  , _chFormatVersion = 1
  , _chTitle = "Design of experiment"
  , _chText = "Looong long text"
  , _chQuestions = qs
  }
