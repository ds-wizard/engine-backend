module Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- ANSWERS
-- -----------------------------------------------------------------
answerNo1 :: Answer
answerNo1 =
  Answer
  { _answerUuid = fromJust $ U.fromString "33da0831-11dd-4faa-b754-41ed98dedcb5"
  , _answerLabel = "No"
  , _answerAdvice = Just "Super long advice"
  , _answerFollowUps = []
  }

answerNo2 :: Answer
answerNo2 =
  Answer
  { _answerUuid = fromJust $ U.fromString "12711c8c-193a-4baf-a071-53f2d3990083"
  , _answerLabel = "No"
  , _answerAdvice = Just "Super long advice"
  , _answerFollowUps = []
  }

answerNo3 :: Answer
answerNo3 =
  Answer
  { _answerUuid = fromJust $ U.fromString "8ebf2494-80c7-4dbb-a4a1-a14d3387abc0"
  , _answerLabel = "No"
  , _answerAdvice = Just "Super long advice"
  , _answerFollowUps = []
  }

answerNo4 :: Answer
answerNo4 =
  Answer
  { _answerUuid = fromJust $ U.fromString "891ebfe2-27df-433c-af83-03bb26fa2764"
  , _answerLabel = "No"
  , _answerAdvice = Just "Super long advice"
  , _answerFollowUps = []
  }

answerYes1 :: Answer
answerYes1 =
  Answer
  { _answerUuid = fromJust $ U.fromString "d6fb1eb3-3bef-4aac-8491-def68f40ac78"
  , _answerLabel = "Yes"
  , _answerAdvice = Just "Short advice"
  , _answerFollowUps = [followUpQuestion1]
  }

answerYes1Changed :: Answer
answerYes1Changed =
  Answer
  { _answerUuid = answerYes1 ^. uuid
  , _answerLabel = "EDITED: Yes"
  , _answerAdvice = Just "EDITED: Short advice"
  , _answerFollowUps = []
  }

answerYes2 :: Answer
answerYes2 =
  Answer
  { _answerUuid = fromJust $ U.fromString "28d49dbe-4180-49c9-80b2-397e9ea27c77"
  , _answerLabel = "Yes"
  , _answerAdvice = Just "Short advice"
  , _answerFollowUps = []
  }

answerYes3 :: Answer
answerYes3 =
  Answer
  { _answerUuid = fromJust $ U.fromString "4d164317-d900-460c-8582-8c80e6d66dcd"
  , _answerLabel = "Yes"
  , _answerAdvice = Just "Short advice"
  , _answerFollowUps = [followUpQuestion2]
  }

answerYes4 :: Answer
answerYes4 =
  Answer
  { _answerUuid = fromJust $ U.fromString "b6b40918-a9b7-4d2d-bacb-9f9aa5683efe"
  , _answerLabel = "Yes"
  , _answerAdvice = Just "Short advice"
  , _answerFollowUps = []
  }

answerMaybe :: Answer
answerMaybe =
  Answer
  { _answerUuid = fromJust $ U.fromString "1f172f5e-3d66-4a1c-a785-85ba02fcf72a"
  , _answerLabel = "Maybe"
  , _answerAdvice = Just "Great advice"
  , _answerFollowUps = []
  }

-- -----------------------------------------------------------------
-- FOLLOW-UP QUESTIONS
-- -----------------------------------------------------------------
followUpQuestion1 :: Question
followUpQuestion1 =
  Question
  { _questionUuid = fromJust $ U.fromString "f9b380eb-bc18-4445-a9bf-14d9a1512d3f"
  , _questionShortUuid = Just "followUpQuestion1"
  , _questionQType = QuestionTypeOption
  , _questionTitle = "First Follow-Up Question"
  , _questionText = "Maybe there will be some description"
  , _questionAnswers = Just [answerNo3, answerYes3]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

followUpQuestion1Plain :: Question
followUpQuestion1Plain =
  Question
  { _questionUuid = followUpQuestion1 ^. uuid
  , _questionShortUuid = followUpQuestion1 ^. shortUuid
  , _questionQType = QuestionTypeOption
  , _questionTitle = "Fourth Question"
  , _questionText = "Just follow"
  , _questionAnswers = Just []
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

followUpQuestion2 :: Question
followUpQuestion2 =
  Question
  { _questionUuid = fromJust $ U.fromString "393eb40a-27bd-4156-9b2d-c4e8c582cca8"
  , _questionShortUuid = Just "followUpQuestion2"
  , _questionQType = QuestionTypeOption
  , _questionTitle = "Second Follow-Up Question"
  , _questionText = "Again just follow"
  , _questionAnswers = Just [answerNo4, answerYes4]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

followUpQuestion2Changed :: Question
followUpQuestion2Changed =
  Question
  { _questionUuid = followUpQuestion2 ^. uuid
  , _questionShortUuid = followUpQuestion2 ^. shortUuid
  , _questionQType = QuestionTypeOption
  , _questionTitle = "EDITED: Second Follow-Up Question"
  , _questionText = "EDITED: Again just follow"
  , _questionAnswers = Just [answerYes4, answerNo4]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

followUpQuestion3 :: Question
followUpQuestion3 =
  Question
  { _questionUuid = fromJust $ U.fromString "70b6a446-bd35-4d5e-8995-78a94a69da83"
  , _questionShortUuid = Just "followUpQuestion3"
  , _questionQType = QuestionTypeOption
  , _questionTitle = "Third Follow-Up Question"
  , _questionText = "Again and again just follow"
  , _questionAnswers = Just []
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }
