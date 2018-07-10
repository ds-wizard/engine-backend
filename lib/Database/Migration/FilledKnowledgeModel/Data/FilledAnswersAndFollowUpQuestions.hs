module Database.Migration.FilledKnowledgeModel.Data.FilledAnswersAndFollowUpQuestions where

import Control.Lens ((^.))

import Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions
import Database.Migration.Branch.Data.KnowledgeModel.Questions
import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

fQ2_answerYes =
  FilledAnswer
  { _filledAnswerUuid = q2_answerYes ^. uuid
  , _filledAnswerLabel = q2_answerYes ^. label
  , _filledAnswerAdvice = q2_answerYes ^. advice
  , _filledAnswerFollowUps = [fQ2_aYes_fuQuestion1]
  }

fQ2_aYes_fuQuestion1 =
  FilledQuestion
  { _filledQuestionUuid = q2_aYes_fuQuestion1 ^. uuid
  , _filledQuestionQType = q2_aYes_fuQuestion1 ^. qType
  , _filledQuestionTitle = q2_aYes_fuQuestion1 ^. title
  , _filledQuestionText = q2_aYes_fuQuestion1 ^. text
  , _filledQuestionAnswerItemTemplate = q2_aYes_fuQuestion1 ^. answerItemTemplate
  , _filledQuestionAnswers = q2_aYes_fuQuestion1 ^. answers
  , _filledQuestionAnswerValue = Nothing
  , _filledQuestionAnswerOption = Just fQ2_aYes_fuq1_answerNo
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = q2_aYes_fuQuestion1 ^. experts
  , _filledQuestionReferences = q2_aYes_fuQuestion1 ^. references
  }

fQ2_aYes_fuq1_answerNo =
  FilledAnswer
  { _filledAnswerUuid = q2_aYes_fuq1_answerNo ^. uuid
  , _filledAnswerLabel = q2_aYes_fuq1_answerNo ^. label
  , _filledAnswerAdvice = q2_aYes_fuq1_answerNo ^. advice
  , _filledAnswerFollowUps = []
  }

-- -------------------------------------------------------
-- -------------------------------------------------------
fQ3_answerNo =
  FilledAnswer
  { _filledAnswerUuid = q3_answerNo ^. uuid
  , _filledAnswerLabel = q3_answerNo ^. label
  , _filledAnswerAdvice = q3_answerNo ^. advice
  , _filledAnswerFollowUps = []
  }

-- -------------------------------------------------------
-- -------------------------------------------------------
fQ4_ait1 =
  FilledAnswerItem
  { _filledAnswerItemTitle = q4_ait ^. title
  , _filledAnswerItemValue = "Ait1: First item"
  , _filledAnswerItemQuestions = [fQ4_ait1_question5, fQ4_ait1_question6]
  }

-- -------------------------------------------------------
fQ4_ait1_question5 =
  FilledQuestion
  { _filledQuestionUuid = q4_ait1_question5 ^. uuid
  , _filledQuestionQType = q4_ait1_question5 ^. qType
  , _filledQuestionTitle = q4_ait1_question5 ^. title
  , _filledQuestionText = q4_ait1_question5 ^. text
  , _filledQuestionAnswerItemTemplate = q4_ait1_question5 ^. answerItemTemplate
  , _filledQuestionAnswers = q4_ait1_question5 ^. answers
  , _filledQuestionAnswerValue = Nothing
  , _filledQuestionAnswerOption = Nothing
  , _filledQuestionAnswerItems = Just [fQ4_ait1_q5_ait1]
  , _filledQuestionExperts = q4_ait1_question5 ^. experts
  , _filledQuestionReferences = q4_ait1_question5 ^. references
  }

fQ4_ait1_q5_ait1 =
  FilledAnswerItem
  { _filledAnswerItemTitle = q4_ait_q5_ait ^. title
  , _filledAnswerItemValue = "Ait1: q5: Ait1: First item"
  , _filledAnswerItemQuestions = [fQ4_ait1_q5_ait1_question7, fQ4_ait1_q5_ait1_question8]
  }

fQ4_ait1_q5_ait1_question7 =
  FilledQuestion
  { _filledQuestionUuid = q4_ait1_q5_ait2_question7 ^. uuid
  , _filledQuestionQType = q4_ait1_q5_ait2_question7 ^. qType
  , _filledQuestionTitle = q4_ait1_q5_ait2_question7 ^. title
  , _filledQuestionText = q4_ait1_q5_ait2_question7 ^. text
  , _filledQuestionAnswerItemTemplate = q4_ait1_q5_ait2_question7 ^. answerItemTemplate
  , _filledQuestionAnswers = q4_ait1_q5_ait2_question7 ^. answers
  , _filledQuestionAnswerValue = Just "Ait1: q5: Ait1: Reply to 7th question"
  , _filledQuestionAnswerOption = Nothing
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = q4_ait1_q5_ait2_question7 ^. experts
  , _filledQuestionReferences = q4_ait1_q5_ait2_question7 ^. references
  }

fQ4_ait1_q5_ait1_question8 =
  FilledQuestion
  { _filledQuestionUuid = q4_ait1_q5_ait2_question8 ^. uuid
  , _filledQuestionQType = q4_ait1_q5_ait2_question8 ^. qType
  , _filledQuestionTitle = q4_ait1_q5_ait2_question8 ^. title
  , _filledQuestionText = q4_ait1_q5_ait2_question8 ^. text
  , _filledQuestionAnswerItemTemplate = q4_ait1_q5_ait2_question8 ^. answerItemTemplate
  , _filledQuestionAnswers = q4_ait1_q5_ait2_question8 ^. answers
  , _filledQuestionAnswerValue = Just "Ait1: q5: Ait1: Reply to 8th question"
  , _filledQuestionAnswerOption = Nothing
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = q4_ait1_q5_ait2_question8 ^. experts
  , _filledQuestionReferences = q4_ait1_q5_ait2_question8 ^. references
  }

-- -------------------------------------------------------
fQ4_ait1_question6 =
  FilledQuestion
  { _filledQuestionUuid = q4_ait1_question6 ^. uuid
  , _filledQuestionQType = q4_ait1_question6 ^. qType
  , _filledQuestionTitle = q4_ait1_question6 ^. title
  , _filledQuestionText = q4_ait1_question6 ^. text
  , _filledQuestionAnswerItemTemplate = q4_ait1_question6 ^. answerItemTemplate
  , _filledQuestionAnswers = q4_ait1_question6 ^. answers
  , _filledQuestionAnswerValue = Nothing
  , _filledQuestionAnswerOption = Just fQ4_ait1_q6_answerNo
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = q4_ait1_question6 ^. experts
  , _filledQuestionReferences = q4_ait1_question6 ^. references
  }

fQ4_ait1_q6_answerNo =
  FilledAnswer
  { _filledAnswerUuid = q4_ait1_q6_answerNo ^. uuid
  , _filledAnswerLabel = q4_ait1_q6_answerNo ^. label
  , _filledAnswerAdvice = q4_ait1_q6_answerNo ^. advice
  , _filledAnswerFollowUps = []
  }

-- -------------------------------------------------------
-- -------------------------------------------------------
fQ4_ait2 =
  FilledAnswerItem
  { _filledAnswerItemTitle = q4_ait ^. title
  , _filledAnswerItemValue = "Ait 2: Second item"
  , _filledAnswerItemQuestions = [fQ4_ait2_question5, fQ4_ait2_question6]
  }

-- -------------------------------------------------------
fQ4_ait2_question5 =
  FilledQuestion
  { _filledQuestionUuid = q4_ait1_question5 ^. uuid
  , _filledQuestionQType = q4_ait1_question5 ^. qType
  , _filledQuestionTitle = q4_ait1_question5 ^. title
  , _filledQuestionText = q4_ait1_question5 ^. text
  , _filledQuestionAnswerItemTemplate = q4_ait1_question5 ^. answerItemTemplate
  , _filledQuestionAnswers = q4_ait1_question5 ^. answers
  , _filledQuestionAnswerValue = Nothing
  , _filledQuestionAnswerOption = Nothing
  , _filledQuestionAnswerItems = Just [fQ4_ait2_q5_ait1]
  , _filledQuestionExperts = q4_ait1_question5 ^. experts
  , _filledQuestionReferences = q4_ait1_question5 ^. references
  }

fQ4_ait2_q5_ait1 =
  FilledAnswerItem
  { _filledAnswerItemTitle = q4_ait_q5_ait ^. title
  , _filledAnswerItemValue = "Ait2: q5: Ait 1: First item"
  , _filledAnswerItemQuestions = [fQ4_ait2_q5_ait1_question7, fQ4_ait2_q5_ait1_question8]
  }

fQ4_ait2_q5_ait1_question7 =
  FilledQuestion
  { _filledQuestionUuid = q4_ait1_q5_ait2_question7 ^. uuid
  , _filledQuestionQType = q4_ait1_q5_ait2_question7 ^. qType
  , _filledQuestionTitle = q4_ait1_q5_ait2_question7 ^. title
  , _filledQuestionText = q4_ait1_q5_ait2_question7 ^. text
  , _filledQuestionAnswerItemTemplate = q4_ait1_q5_ait2_question7 ^. answerItemTemplate
  , _filledQuestionAnswers = q4_ait1_q5_ait2_question7 ^. answers
  , _filledQuestionAnswerValue = Just "Ait2: q5: Ait1: Reply to 7th question"
  , _filledQuestionAnswerOption = Nothing
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = q4_ait1_q5_ait2_question7 ^. experts
  , _filledQuestionReferences = q4_ait1_q5_ait2_question7 ^. references
  }

fQ4_ait2_q5_ait1_question8 =
  FilledQuestion
  { _filledQuestionUuid = q4_ait1_q5_ait2_question8 ^. uuid
  , _filledQuestionQType = q4_ait1_q5_ait2_question8 ^. qType
  , _filledQuestionTitle = q4_ait1_q5_ait2_question8 ^. title
  , _filledQuestionText = q4_ait1_q5_ait2_question8 ^. text
  , _filledQuestionAnswerItemTemplate = q4_ait1_q5_ait2_question8 ^. answerItemTemplate
  , _filledQuestionAnswers = q4_ait1_q5_ait2_question8 ^. answers
  , _filledQuestionAnswerValue = Just "Ait2: q5: Ait1: Reply to 8th question"
  , _filledQuestionAnswerOption = Nothing
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = q4_ait1_q5_ait2_question8 ^. experts
  , _filledQuestionReferences = q4_ait1_q5_ait2_question8 ^. references
  }

-- -------------------------------------------------------
fQ4_ait2_question6 =
  FilledQuestion
  { _filledQuestionUuid = q4_ait1_question6 ^. uuid
  , _filledQuestionQType = q4_ait1_question6 ^. qType
  , _filledQuestionTitle = q4_ait1_question6 ^. title
  , _filledQuestionText = q4_ait1_question6 ^. text
  , _filledQuestionAnswerItemTemplate = q4_ait1_question6 ^. answerItemTemplate
  , _filledQuestionAnswers = q4_ait1_question6 ^. answers
  , _filledQuestionAnswerValue = Nothing
  , _filledQuestionAnswerOption = Just fQ4_ait1_q6_answerNo
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = q4_ait1_question6 ^. experts
  , _filledQuestionReferences = q4_ait1_question6 ^. references
  }

fQ4_ait2_q6_answerNo =
  FilledAnswer
  { _filledAnswerUuid = q4_ait1_q6_answerNo ^. uuid
  , _filledAnswerLabel = q4_ait1_q6_answerNo ^. label
  , _filledAnswerAdvice = q4_ait1_q6_answerNo ^. advice
  , _filledAnswerFollowUps = []
  }
