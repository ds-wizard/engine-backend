module Fixtures.KnowledgeModel.AnswersAndFollowUpQuestions where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- ANSWERS
-- -----------------------------------------------------------------
answerNo1 :: Answer
answerNo1 =
 Answer
 { _ansUuid = fromJust $ U.fromString "33da0831-11dd-4faa-b754-41ed98dedcb5"
 , _ansLabel = "No"
 , _ansAdvice = Just "Super long advice"
 , _ansFollowing = []
 }

answerNo2 :: Answer
answerNo2 =
 Answer
 { _ansUuid = fromJust $ U.fromString "12711c8c-193a-4baf-a071-53f2d3990083"
 , _ansLabel = "No"
 , _ansAdvice = Just "Super long advice"
 , _ansFollowing = []
 }

answerNo3 :: Answer
answerNo3 =
 Answer
 { _ansUuid = fromJust $ U.fromString "8ebf2494-80c7-4dbb-a4a1-a14d3387abc0"
 , _ansLabel = "No"
 , _ansAdvice = Just "Super long advice"
 , _ansFollowing = []
 }

answerNo4 :: Answer
answerNo4 =
 Answer
 { _ansUuid = fromJust $ U.fromString "891ebfe2-27df-433c-af83-03bb26fa2764"
 , _ansLabel = "No"
 , _ansAdvice = Just "Super long advice"
 , _ansFollowing = []
 }

answerYes1 :: Answer
answerYes1 =
 Answer
 { _ansUuid = fromJust $ U.fromString "d6fb1eb3-3bef-4aac-8491-def68f40ac78"
 , _ansLabel = "Yes"
 , _ansAdvice = Just "Short advice"
 , _ansFollowing = [followUpQuestion1]
 }

answerYes1Changed :: Answer
answerYes1Changed =
 Answer
 { _ansUuid = answerYes1 ^. ansUuid
 , _ansLabel = "EDITED: Yes"
 , _ansAdvice = Just "EDITED: Short advice"
 , _ansFollowing = []
 }

answerYes2 :: Answer
answerYes2 =
 Answer
 { _ansUuid = fromJust $ U.fromString "28d49dbe-4180-49c9-80b2-397e9ea27c77"
 , _ansLabel = "Yes"
 , _ansAdvice = Just "Short advice"
 , _ansFollowing = []
 }

answerYes3 :: Answer
answerYes3 =
 Answer
 { _ansUuid = fromJust $ U.fromString "4d164317-d900-460c-8582-8c80e6d66dcd"
 , _ansLabel = "Yes"
 , _ansAdvice = Just "Short advice"
 , _ansFollowing = [followUpQuestion2]
 }

answerYes4 :: Answer
answerYes4 =
 Answer
 { _ansUuid = fromJust $ U.fromString "b6b40918-a9b7-4d2d-bacb-9f9aa5683efe"
 , _ansLabel = "Yes"
 , _ansAdvice = Just "Short advice"
 , _ansFollowing = []
 }

answerMaybe :: Answer
answerMaybe =
 Answer
 { _ansUuid = fromJust $ U.fromString "1f172f5e-3d66-4a1c-a785-85ba02fcf72a"
 , _ansLabel = "Maybe"
 , _ansAdvice = Just "Great advice"
 , _ansFollowing = []
 }

-- -----------------------------------------------------------------
-- FOLLOW-UP QUESTIONS
-- -----------------------------------------------------------------
fuqTypeOption :: String
fuqTypeOption = "option"

fuqTypeList :: String
fuqTypeList = "list"

followUpQuestion1 :: Question
followUpQuestion1 =
 Question
 { _qUuid = fromJust $ U.fromString "f9b380eb-bc18-4445-a9bf-14d9a1512d3f"
 -- , _qNamespace = FC.namespaceCore
 , _qType = fuqTypeOption
 , _qTitle = "First Follow-Up Question"
 , _qText = "Maybe there will be some description"
 , _qAnswers = [answerNo3, answerYes3]
 , _qReferences = []
 , _qExperts = []
 }

followUpQuestion1Plain :: Question
followUpQuestion1Plain =
 Question
 { _qUuid = followUpQuestion1 ^. qUuid
 -- , _qNamespace = FC.namespaceCore
 , _qType = fuqTypeOption
 , _qTitle = "Fourth Question"
 , _qText = "Just follow"
 , _qAnswers = []
 , _qReferences = []
 , _qExperts = []
 }

followUpQuestion2 :: Question
followUpQuestion2 =
 Question
 { _qUuid = fromJust $ U.fromString "393eb40a-27bd-4156-9b2d-c4e8c582cca8"
 -- , _qNamespace = FC.namespaceCore
 , _qType = fuqTypeOption
 , _qTitle = "Second Follow-Up Question"
 , _qText = "Again just follow"
 , _qAnswers = [answerNo4, answerYes4]
 , _qReferences = []
 , _qExperts = []
 }

followUpQuestion2Changed :: Question
followUpQuestion2Changed =
 Question
 { _qUuid = followUpQuestion2 ^. qUuid
 -- , _qNamespace = FC.namespaceCore
 , _qType = fuqTypeOption
 , _qTitle = "EDITED: Second Follow-Up Question"
 , _qText = "EDITED: Again just follow"
 , _qAnswers = [answerYes4, answerNo4]
 , _qReferences = []
 , _qExperts = []
 }

followUpQuestion3 :: Question
followUpQuestion3 =
 Question
 { _qUuid = fromJust $ U.fromString "70b6a446-bd35-4d5e-8995-78a94a69da83"
   -- , _qNamespace = FC.namespaceCore
 , _qType = fuqTypeOption
 , _qTitle = "Third Follow-Up Question"
 , _qText = "Again and again just follow"
 , _qAnswers = []
 , _qReferences = []
 , _qExperts = []
 }

