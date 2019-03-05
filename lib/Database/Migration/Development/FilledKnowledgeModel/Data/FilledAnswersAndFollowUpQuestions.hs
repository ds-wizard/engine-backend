module Database.Migration.Development.FilledKnowledgeModel.Data.FilledAnswersAndFollowUpQuestions where

import Control.Lens ((^.))

import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Questions
import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

fQ2_answerYes :: FilledAnswer
fQ2_answerYes =
  FilledAnswer
  { _filledAnswerUuid = q2_answerYes ^. uuid
  , _filledAnswerHumanIdentifier = "b"
  , _filledAnswerLabel = q2_answerYes ^. label
  , _filledAnswerAdvice = q2_answerYes ^. advice
  , _filledAnswerFollowUps = [fQ2_aYes_fuQuestion1']
  , _filledAnswerMetricMeasures = q2_answerYes ^. metricMeasures
  }

fQ2_aYes_fuQuestion1' :: FilledQuestion
fQ2_aYes_fuQuestion1' = FilledOptionsQuestion' fQ2_aYes_fuQuestion1

fQ2_aYes_fuQuestion1 :: FilledOptionsQuestion
fQ2_aYes_fuQuestion1 =
  FilledOptionsQuestion
  { _filledOptionsQuestionUuid = q2_aYes_fuQuestion1 ^. uuid
  , _filledOptionsQuestionHumanIdentifier = "2.b.1"
  , _filledOptionsQuestionTitle = q2_aYes_fuQuestion1 ^. title
  , _filledOptionsQuestionText = q2_aYes_fuQuestion1 ^. text
  , _filledOptionsQuestionRequiredLevel = q2_aYes_fuQuestion1 ^. requiredLevel
  , _filledOptionsQuestionTagUuids = q2_aYes_fuQuestion1 ^. tagUuids
  , _filledOptionsQuestionExperts = q2_aYes_fuQuestion1 ^. experts
  , _filledOptionsQuestionReferences = q2_aYes_fuQuestion1 ^. references
  , _filledOptionsQuestionAnswers = q2_aYes_fuQuestion1 ^. answers
  , _filledOptionsQuestionAnswerOption = Just fQ2_aYes_fuq1_answerNo
  }

fQ2_aYes_fuq1_answerNo :: FilledAnswer
fQ2_aYes_fuq1_answerNo =
  FilledAnswer
  { _filledAnswerUuid = q2_aYes_fuq1_answerNo ^. uuid
  , _filledAnswerHumanIdentifier = "a"
  , _filledAnswerLabel = q2_aYes_fuq1_answerNo ^. label
  , _filledAnswerAdvice = q2_aYes_fuq1_answerNo ^. advice
  , _filledAnswerFollowUps = []
  , _filledAnswerMetricMeasures = q2_aYes_fuq1_answerNo ^. metricMeasures
  }

-- -------------------------------------------------------
-- -------------------------------------------------------
fQ3_answerNo :: FilledAnswer
fQ3_answerNo =
  FilledAnswer
  { _filledAnswerUuid = q3_answerNo ^. uuid
  , _filledAnswerHumanIdentifier = "a"
  , _filledAnswerLabel = q3_answerNo ^. label
  , _filledAnswerAdvice = q3_answerNo ^. advice
  , _filledAnswerFollowUps = []
  , _filledAnswerMetricMeasures = q3_answerNo ^. metricMeasures
  }

-- -------------------------------------------------------
-- -------------------------------------------------------
fQ4_ai1 :: FilledAnswerItem
fQ4_ai1 =
  FilledAnswerItem
  { _filledAnswerItemTitle = question4 ^. itemTemplateTitle
  , _filledAnswerItemHumanIdentifier = "2.a"
  , _filledAnswerItemValue = Just "Ai1: First item"
  , _filledAnswerItemQuestions = [fQ4_it1_question5', fQ4_it1_question6']
  }

-- -------------------------------------------------------
fQ4_it1_question5' :: FilledQuestion
fQ4_it1_question5' = FilledListQuestion' fQ4_it1_question5

fQ4_it1_question5 :: FilledListQuestion
fQ4_it1_question5 =
  FilledListQuestion
  { _filledListQuestionUuid = q4_it1_question5 ^. uuid
  , _filledListQuestionHumanIdentifier = "2.a.1"
  , _filledListQuestionTitle = q4_it1_question5 ^. title
  , _filledListQuestionText = q4_it1_question5 ^. text
  , _filledListQuestionRequiredLevel = q4_it1_question5 ^. requiredLevel
  , _filledListQuestionTagUuids = q4_it1_question5 ^. tagUuids
  , _filledListQuestionExperts = q4_it1_question5 ^. experts
  , _filledListQuestionReferences = q4_it1_question5 ^. references
  , _filledListQuestionItemTemplateTitle = q4_it1_question5 ^. itemTemplateTitle
  , _filledListQuestionItemTemplateQuestions = q4_it1_question5 ^. itemTemplateQuestions
  , _filledListQuestionItems = Just [fQ4_it1_q5_ai1]
  }

fQ4_it1_q5_ai1 :: FilledAnswerItem
fQ4_it1_q5_ai1 =
  FilledAnswerItem
  { _filledAnswerItemTitle = q4_it1_question5 ^. itemTemplateTitle
  , _filledAnswerItemHumanIdentifier = "2.a.1.a"
  , _filledAnswerItemValue = Just "Ai1: q5: Ai1: First item"
  , _filledAnswerItemQuestions = [fQ4_it1_q5_it1_question7', fQ4_it1_q5_it1_question8']
  }

fQ4_it1_q5_it1_question7' :: FilledQuestion
fQ4_it1_q5_it1_question7' = FilledValueQuestion' fQ4_it1_q5_it1_question7

fQ4_it1_q5_it1_question7 :: FilledValueQuestion
fQ4_it1_q5_it1_question7 =
  FilledValueQuestion
  { _filledValueQuestionUuid = q4_it1_q5_it2_question7 ^. uuid
  , _filledValueQuestionHumanIdentifier = "2.a.1.a.1"
  , _filledValueQuestionTitle = q4_it1_q5_it2_question7 ^. title
  , _filledValueQuestionText = q4_it1_q5_it2_question7 ^. text
  , _filledValueQuestionRequiredLevel = q4_it1_q5_it2_question7 ^. requiredLevel
  , _filledValueQuestionTagUuids = q4_it1_q5_it2_question7 ^. tagUuids
  , _filledValueQuestionExperts = q4_it1_q5_it2_question7 ^. experts
  , _filledValueQuestionReferences = q4_it1_q5_it2_question7 ^. references
  , _filledValueQuestionValueType = q4_it1_q5_it2_question7 ^. valueType
  , _filledValueQuestionAnswerValue = Just "Ai1: q5: Ai1: Reply to 7th question"
  }

fQ4_it1_q5_it1_question8' :: FilledQuestion
fQ4_it1_q5_it1_question8' = FilledValueQuestion' fQ4_it1_q5_it1_question8

fQ4_it1_q5_it1_question8 :: FilledValueQuestion
fQ4_it1_q5_it1_question8 =
  FilledValueQuestion
  { _filledValueQuestionUuid = q4_it1_q5_it2_question8 ^. uuid
  , _filledValueQuestionHumanIdentifier = "2.a.1.a.2"
  , _filledValueQuestionTitle = q4_it1_q5_it2_question8 ^. title
  , _filledValueQuestionText = q4_it1_q5_it2_question8 ^. text
  , _filledValueQuestionRequiredLevel = q4_it1_q5_it2_question8 ^. requiredLevel
  , _filledValueQuestionTagUuids = q4_it1_q5_it2_question8 ^. tagUuids
  , _filledValueQuestionExperts = q4_it1_q5_it2_question8 ^. experts
  , _filledValueQuestionReferences = q4_it1_q5_it2_question8 ^. references
  , _filledValueQuestionValueType = q4_it1_q5_it2_question8 ^. valueType
  , _filledValueQuestionAnswerValue = Just "Ai1: q5: Ai1: Reply to 8th question"
  }

-- -------------------------------------------------------
fQ4_it1_question6' :: FilledQuestion
fQ4_it1_question6' = FilledOptionsQuestion' fQ4_it1_question6

fQ4_it1_question6 :: FilledOptionsQuestion
fQ4_it1_question6 =
  FilledOptionsQuestion
  { _filledOptionsQuestionUuid = q4_it1_question6 ^. uuid
  , _filledOptionsQuestionHumanIdentifier = "2.a.2"
  , _filledOptionsQuestionTitle = q4_it1_question6 ^. title
  , _filledOptionsQuestionText = q4_it1_question6 ^. text
  , _filledOptionsQuestionRequiredLevel = q4_it1_question6 ^. requiredLevel
  , _filledOptionsQuestionTagUuids = q4_it1_question6 ^. tagUuids
  , _filledOptionsQuestionExperts = q4_it1_question6 ^. experts
  , _filledOptionsQuestionReferences = q4_it1_question6 ^. references
  , _filledOptionsQuestionAnswers = q4_it1_question6 ^. answers
  , _filledOptionsQuestionAnswerOption = Just fQ4_it1_q6_answerNo
  }

fQ4_it1_q6_answerNo :: FilledAnswer
fQ4_it1_q6_answerNo =
  FilledAnswer
  { _filledAnswerUuid = q4_it1_q6_answerNo ^. uuid
  , _filledAnswerHumanIdentifier = "a"
  , _filledAnswerLabel = q4_it1_q6_answerNo ^. label
  , _filledAnswerAdvice = q4_it1_q6_answerNo ^. advice
  , _filledAnswerFollowUps = []
  , _filledAnswerMetricMeasures = q4_it1_q6_answerNo ^. metricMeasures
  }

-- -------------------------------------------------------
-- -------------------------------------------------------
fQ4_ai2 :: FilledAnswerItem
fQ4_ai2 =
  FilledAnswerItem
  { _filledAnswerItemTitle = question4 ^. itemTemplateTitle
  , _filledAnswerItemHumanIdentifier = "2.b"
  , _filledAnswerItemValue = Just "Ai 2: Second item"
  , _filledAnswerItemQuestions = [fQ4_it2_question5', fQ4_it2_question6']
  }

-- -------------------------------------------------------
fQ4_it2_question5' :: FilledQuestion
fQ4_it2_question5' = FilledListQuestion' fQ4_it2_question5

fQ4_it2_question5 :: FilledListQuestion
fQ4_it2_question5 =
  FilledListQuestion
  { _filledListQuestionUuid = q4_it1_question5 ^. uuid
  , _filledListQuestionHumanIdentifier = "2.b.1"
  , _filledListQuestionTitle = q4_it1_question5 ^. title
  , _filledListQuestionText = q4_it1_question5 ^. text
  , _filledListQuestionRequiredLevel = q4_it1_question5 ^. requiredLevel
  , _filledListQuestionTagUuids = q4_it1_question5 ^. tagUuids
  , _filledListQuestionExperts = q4_it1_question5 ^. experts
  , _filledListQuestionReferences = q4_it1_question5 ^. references
  , _filledListQuestionItemTemplateTitle = q4_it1_question5 ^. itemTemplateTitle
  , _filledListQuestionItemTemplateQuestions = q4_it1_question5 ^. itemTemplateQuestions
  , _filledListQuestionItems = Just []
  }

-- -------------------------------------------------------
fQ4_it2_question6' :: FilledQuestion
fQ4_it2_question6' = FilledOptionsQuestion' fQ4_it2_question6

fQ4_it2_question6 :: FilledOptionsQuestion
fQ4_it2_question6 =
  FilledOptionsQuestion
  { _filledOptionsQuestionUuid = q4_it1_question6 ^. uuid
  , _filledOptionsQuestionHumanIdentifier = "2.b.2"
  , _filledOptionsQuestionTitle = q4_it1_question6 ^. title
  , _filledOptionsQuestionText = q4_it1_question6 ^. text
  , _filledOptionsQuestionRequiredLevel = q4_it1_question6 ^. requiredLevel
  , _filledOptionsQuestionTagUuids = q4_it1_question6 ^. tagUuids
  , _filledOptionsQuestionExperts = q4_it1_question6 ^. experts
  , _filledOptionsQuestionReferences = q4_it1_question6 ^. references
  , _filledOptionsQuestionAnswers = q4_it1_question6 ^. answers
  , _filledOptionsQuestionAnswerOption = Just fQ4_it1_q6_answerNo
  }

fQ4_it2_q6_answerNo :: FilledAnswer
fQ4_it2_q6_answerNo =
  FilledAnswer
  { _filledAnswerUuid = q4_it1_q6_answerNo ^. uuid
  , _filledAnswerHumanIdentifier = "b"
  , _filledAnswerLabel = q4_it1_q6_answerNo ^. label
  , _filledAnswerAdvice = q4_it1_q6_answerNo ^. advice
  , _filledAnswerFollowUps = []
  , _filledAnswerMetricMeasures = q4_it1_q6_answerNo ^. metricMeasures
  }
