module Fixtures.KnowledgeModel.AnswersAndFollowUpQuestions where

import Data.Maybe

import Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- ANSWERS
-- -----------------------------------------------------------------
answerNo1 =
  Answer
  { _ansUuid = "answerNo1"
  , _ansLabel = "No"
  , _ansAdvice = Just "Super long advice"
  , _ansFollowing = []
  }

answerNo2 =
  Answer
  { _ansUuid = "answerNo2"
  , _ansLabel = "No"
  , _ansAdvice = Just "Super long advice"
  , _ansFollowing = []
  }

answerNo3 =
  Answer
  { _ansUuid = "answerNo3"
  , _ansLabel = "No"
  , _ansAdvice = Just "Super long advice"
  , _ansFollowing = []
  }

answerNo4 =
  Answer
  { _ansUuid = "answerNo4"
  , _ansLabel = "No"
  , _ansAdvice = Just "Super long advice"
  , _ansFollowing = []
  }

answerYes1 =
  Answer
  { _ansUuid = "answerYes1"
  , _ansLabel = "Yes"
  , _ansAdvice = Just "Short advice"
  , _ansFollowing = [followUpQuestion1]
  }

answerYes1Changed =
  Answer
  { _ansUuid = "answerYes1"
  , _ansLabel = "EDITED: Yes"
  , _ansAdvice = Just "EDITED: Short advice"
  , _ansFollowing = []
  }

answerYes2 =
  Answer
  { _ansUuid = "answerYes2"
  , _ansLabel = "Yes"
  , _ansAdvice = Just "Short advice"
  , _ansFollowing = []
  }

answerYes3 =
  Answer
  { _ansUuid = "answerYes3"
  , _ansLabel = "Yes"
  , _ansAdvice = Just "Short advice"
  , _ansFollowing = [followUpQuestion2]
  }

answerYes4 =
  Answer
  { _ansUuid = "answerYes4"
  , _ansLabel = "Yes"
  , _ansAdvice = Just "Short advice"
  , _ansFollowing = []
  }

answerMaybe =
  Answer
  { _ansUuid = "answerMaybe"
  , _ansLabel = "Maybe"
  , _ansAdvice = Just "Great advice"
  , _ansFollowing = []
  }

-- -----------------------------------------------------------------
-- FOLLOW-UP QUESTIONS
-- -----------------------------------------------------------------
fuqTypeOption = "option"

fuqTypeList = "list"

followUpQuestion1 =
  Question
  { _qUuid = "followUpQuestion1"
  -- , _qNamespace = FC.namespaceCore
  , _qType = fuqTypeOption
  , _qTitle = "First Follow-Up Question"
  , _qText = "Maybe there will be some description"
  , _qAnswers = [answerNo3, answerYes3]
  , _qReferences = []
  , _qExperts = []
  }

followUpQuestion1Plain =
  Question
  { _qUuid = "followUpQuestion1"
  -- , _qNamespace = FC.namespaceCore
  , _qType = fuqTypeOption
  , _qTitle = "Fourth Question"
  , _qText = "Just follow"
  , _qAnswers = []
  , _qReferences = []
  , _qExperts = []
  }

followUpQuestion2 =
  Question
  { _qUuid = "followUpQuestion2"
  -- , _qNamespace = FC.namespaceCore
  , _qType = fuqTypeOption
  , _qTitle = "Second Follow-Up Question"
  , _qText = "Again just follow"
  , _qAnswers = [answerNo4, answerYes4]
  , _qReferences = []
  , _qExperts = []
  }

followUpQuestion2Changed =
  Question
  { _qUuid = "followUpQuestion2"
  -- , _qNamespace = FC.namespaceCore
  , _qType = fuqTypeOption
  , _qTitle = "EDITED: Second Follow-Up Question"
  , _qText = "EDITED: Again just follow"
  , _qAnswers = [answerYes4, answerNo4]
  , _qReferences = []
  , _qExperts = []
  }

followUpQuestion3 =
  Question
  { _qUuid = "followUpQuestion3"
    -- , _qNamespace = FC.namespaceCore
  , _qType = fuqTypeOption
  , _qTitle = "Third Follow-Up Question"
  , _qText = "Again and again just follow"
  , _qAnswers = []
  , _qReferences = []
  , _qExperts = []
  }

