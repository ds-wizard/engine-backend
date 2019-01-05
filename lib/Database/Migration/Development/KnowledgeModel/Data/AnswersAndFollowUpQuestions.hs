module Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U

import Database.Migration.Development.KnowledgeModel.Data.MetricMeasures
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

-- -----------------------------------------------------------------
-- ANSWERS
-- -----------------------------------------------------------------
q2_answerNo :: Answer
q2_answerNo =
  Answer
  { _answerUuid = fromJust $ U.fromString "33da0831-11dd-4faa-b754-41ed98dedcb5"
  , _answerLabel = "No"
  , _answerAdvice = Just "Super long advice"
  , _answerFollowUps = []
  , _answerMetricMeasures = [metricMeasureF1, metricMeasureA1]
  }

q3_answerNo :: Answer
q3_answerNo =
  Answer
  { _answerUuid = fromJust $ U.fromString "12711c8c-193a-4baf-a071-53f2d3990083"
  , _answerLabel = "No"
  , _answerAdvice = Just "Super long advice"
  , _answerFollowUps = []
  , _answerMetricMeasures = [metricMeasureF1, metricMeasureA1]
  }

q4_ait1_q6_answerNo :: Answer
q4_ait1_q6_answerNo =
  Answer
  { _answerUuid = fromJust $ U.fromString "a093c2c3-123c-42ee-9667-13af14b6249e"
  , _answerLabel = "No"
  , _answerAdvice = Just "Great advice"
  , _answerFollowUps = []
  , _answerMetricMeasures = []
  }

q2_aYes_fuq1_answerNo :: Answer
q2_aYes_fuq1_answerNo =
  Answer
  { _answerUuid = fromJust $ U.fromString "8ebf2494-80c7-4dbb-a4a1-a14d3387abc0"
  , _answerLabel = "No"
  , _answerAdvice = Just "Super long advice"
  , _answerFollowUps = []
  , _answerMetricMeasures = []
  }

q2_aYes_fuq1_aYes_fuq2_answerNo :: Answer
q2_aYes_fuq1_aYes_fuq2_answerNo =
  Answer
  { _answerUuid = fromJust $ U.fromString "891ebfe2-27df-433c-af83-03bb26fa2764"
  , _answerLabel = "No"
  , _answerAdvice = Just "Super long advice"
  , _answerFollowUps = []
  , _answerMetricMeasures = []
  }

q2_answerYes :: Answer
q2_answerYes =
  Answer
  { _answerUuid = fromJust $ U.fromString "d6fb1eb3-3bef-4aac-8491-def68f40ac78"
  , _answerLabel = "Yes"
  , _answerAdvice =
      Just
        "You know that this is very unlikely? This question is not only about data sets that are similar to what you want to determine yourself, but also reference data or data that should be mined from the existing literature. Further, it is very likely that you will refer to related data, e.g. other databases where you usually \"quickly look something up\", but that could maybe be properly integrated, especially if you need to do such lookups multiple times."
  , _answerFollowUps = [q2_aYes_fuQuestion1]
  , _answerMetricMeasures = [metricMeasureI1, metricMeasureR1]
  }

q2_answerYesChanged :: Answer
q2_answerYesChanged =
  Answer
  { _answerUuid = q2_answerYes ^. uuid
  , _answerLabel = "EDITED: Yes"
  , _answerAdvice = Just "EDITED: Short advice"
  , _answerFollowUps = []
  , _answerMetricMeasures = [metricMeasureI1, metricMeasureR1, metricMeasureG1]
  }

q3_answerYes :: Answer
q3_answerYes =
  Answer
  { _answerUuid = fromJust $ U.fromString "28d49dbe-4180-49c9-80b2-397e9ea27c77"
  , _answerLabel = "Yes"
  , _answerAdvice = Just "Short advice"
  , _answerFollowUps = []
  , _answerMetricMeasures = []
  }

q2_aYes_fuq1_answerYes :: Answer
q2_aYes_fuq1_answerYes =
  Answer
  { _answerUuid = fromJust $ U.fromString "4d164317-d900-460c-8582-8c80e6d66dcd"
  , _answerLabel = "Yes"
  , _answerAdvice = Just "Short advice"
  , _answerFollowUps = [q2_aYes_fuq1_aYes_fuQuestion2]
  , _answerMetricMeasures = []
  }

q2_aYes_fuq1_aYes_fuq2_answerYes :: Answer
q2_aYes_fuq1_aYes_fuq2_answerYes =
  Answer
  { _answerUuid = fromJust $ U.fromString "b6b40918-a9b7-4d2d-bacb-9f9aa5683efe"
  , _answerLabel = "Yes"
  , _answerAdvice = Just "Short advice"
  , _answerFollowUps = []
  , _answerMetricMeasures = []
  }

q4_ait1_q6_answerYes :: Answer
q4_ait1_q6_answerYes =
  Answer
  { _answerUuid = fromJust $ U.fromString "16f20d73-b335-47d8-8d35-157e8c3cd009"
  , _answerLabel = "Yes"
  , _answerAdvice = Just "Short advice"
  , _answerFollowUps = [q4_ait1_q6_aYes_followUpQuestion4]
  , _answerMetricMeasures = []
  }

q2_answerMaybe :: Answer
q2_answerMaybe =
  Answer
  { _answerUuid = fromJust $ U.fromString "1f172f5e-3d66-4a1c-a785-85ba02fcf72a"
  , _answerLabel = "Maybe"
  , _answerAdvice = Just "Great advice"
  , _answerFollowUps = []
  , _answerMetricMeasures = []
  }

-- -----------------------------------------------------------------
-- FOLLOW-UP QUESTIONS
-- -----------------------------------------------------------------
q2_aYes_fuQuestion1 :: Question
q2_aYes_fuQuestion1 =
  Question
  { _questionUuid = fromJust $ U.fromString "f9b380eb-bc18-4445-a9bf-14d9a1512d3f"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "First Follow-Up Question"
  , _questionText = Just "Maybe there will be some description"
  , _questionRequiredLevel = Just 2
  , _questionAnswers = Just [q2_aYes_fuq1_answerNo, q2_aYes_fuq1_answerYes]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

q2_aYes_fuQuestion1Plain :: Question
q2_aYes_fuQuestion1Plain =
  Question
  { _questionUuid = q2_aYes_fuQuestion1 ^. uuid
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Fourth Question"
  , _questionText = Just "Just follow"
  , _questionRequiredLevel = Just 2
  , _questionAnswers = Just []
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

-- -----------------------------------------------------------------------------
q2_aYes_fuq1_aYes_fuQuestion2 :: Question
q2_aYes_fuq1_aYes_fuQuestion2 =
  Question
  { _questionUuid = fromJust $ U.fromString "393eb40a-27bd-4156-9b2d-c4e8c582cca8"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Second Follow-Up Question"
  , _questionText = Just "Again just follow"
  , _questionRequiredLevel = Just 2
  , _questionAnswers = Just [q2_aYes_fuq1_aYes_fuq2_answerNo, q2_aYes_fuq1_aYes_fuq2_answerYes]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

q2_aYes_fuq1_aYes_fuQuestion2Changed :: Question
q2_aYes_fuq1_aYes_fuQuestion2Changed =
  Question
  { _questionUuid = q2_aYes_fuq1_aYes_fuQuestion2 ^. uuid
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "EDITED: Second Follow-Up Question"
  , _questionText = Just "EDITED: Again just follow"
  , _questionRequiredLevel = Just 1
  , _questionAnswers = Just [q2_aYes_fuq1_aYes_fuq2_answerYes, q2_aYes_fuq1_aYes_fuq2_answerNo]
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

-- -----------------------------------------------------------------------------
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 :: Question
q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3 =
  Question
  { _questionUuid = fromJust $ U.fromString "70b6a446-bd35-4d5e-8995-78a94a69da83"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Third Follow-Up Question"
  , _questionText = Just "Again and again just follow"
  , _questionRequiredLevel = Just 2
  , _questionAnswers = Just []
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

-- -----------------------------------------------------------------------------
q4_ait1_q6_aYes_followUpQuestion4 :: Question
q4_ait1_q6_aYes_followUpQuestion4 =
  Question
  { _questionUuid = fromJust $ U.fromString "cd98f76a-a430-4bd6-ba63-eb4c3c5c8c7e"
  , _questionQType = QuestionTypeList
  , _questionTitle = "Fourth Follow-Up Question"
  , _questionText = Just "Again and again just follow"
  , _questionRequiredLevel = Just 2
  , _questionAnswers = Nothing
  , _questionAnswerItemTemplate = Just q4_ait1_q6_aYes_fuq4_ait
  , _questionReferences = []
  , _questionExperts = []
  }

q4_ait1_q6_aYes_fuq4_ait :: AnswerItemTemplate
q4_ait1_q6_aYes_fuq4_ait =
  AnswerItemTemplate
  { _answerItemTemplateTitle = "fup 4 template title"
  , _answerItemTemplateQuestions = [q4_ait1_q6_aYes_fuq4_ait_question1, q4_ait1_q6_aYes_fuq4_ait_question2]
  }

q4_ait1_q6_aYes_followUpQuestion4Changed :: Question
q4_ait1_q6_aYes_followUpQuestion4Changed =
  Question
  { _questionUuid = q4_ait1_q6_aYes_followUpQuestion4 ^. uuid
  , _questionQType = QuestionTypeList
  , _questionTitle = "EDITED: Third Follow-Up Question"
  , _questionText = Just "EDITED: Again and again just follow"
  , _questionRequiredLevel = Just 1
  , _questionAnswers = Nothing
  , _questionAnswerItemTemplate =
      Just
        AnswerItemTemplate
        { _answerItemTemplateTitle = "EDITED: fup 4 template title"
        , _answerItemTemplateQuestions = [q4_ait1_q6_aYes_fuq4_ait_question2, q4_ait1_q6_aYes_fuq4_ait_question1]
        }
  , _questionReferences = []
  , _questionExperts = []
  }

q4_ait1_q6_aYes_fuq4_ait_question1 :: Question
q4_ait1_q6_aYes_fuq4_ait_question1 =
  Question
  { _questionUuid = fromJust $ U.fromString "e5a3e1b2-077a-405f-b35c-3bffded63140"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Sub question 1 of Follow-Up Question 4"
  , _questionText = Just "Again and again just follow"
  , _questionRequiredLevel = Just 2
  , _questionAnswers = Just []
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }

q4_ait1_q6_aYes_fuq4_ait_question2 :: Question
q4_ait1_q6_aYes_fuq4_ait_question2 =
  Question
  { _questionUuid = fromJust $ U.fromString "7f2e3fe5-b8b6-4b5a-812d-c5c1c704b3d9"
  , _questionQType = QuestionTypeOptions
  , _questionTitle = "Sub question 2 of Follow-Up Question 4"
  , _questionText = Just "Again and again just follow"
  , _questionRequiredLevel = Just 2
  , _questionAnswers = Just []
  , _questionAnswerItemTemplate = Nothing
  , _questionReferences = []
  , _questionExperts = []
  }
