module Database.Migration.Development.FilledKnowledgeModel.Data.FilledQuestions where

import Control.Lens ((^.))

import Database.Migration.Development.FilledKnowledgeModel.Data.FilledAnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Questions
import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

fQuestion1 =
  FilledQuestion
  { _filledQuestionUuid = question1 ^. uuid
  , _filledQuestionQType = question1 ^. qType
  , _filledQuestionTitle = question1 ^. title
  , _filledQuestionText = question1 ^. text
  , _filledQuestionRequiredLevel = question1 ^. requiredLevel
  , _filledQuestionTagUuids = question1 ^. tagUuids
  , _filledQuestionAnswerItemTemplate = question1 ^. answerItemTemplate
  , _filledQuestionAnswers = question1 ^. answers
  , _filledQuestionAnswerValue = Just "Reply to 1st question"
  , _filledQuestionAnswerOption = Nothing
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = question1 ^. experts
  , _filledQuestionReferences = question1 ^. references
  }

fQuestion2 =
  FilledQuestion
  { _filledQuestionUuid = question2 ^. uuid
  , _filledQuestionQType = question2 ^. qType
  , _filledQuestionTitle = question2 ^. title
  , _filledQuestionText = question2 ^. text
  , _filledQuestionRequiredLevel = question2 ^. requiredLevel
  , _filledQuestionTagUuids = question2 ^. tagUuids
  , _filledQuestionAnswerItemTemplate = question2 ^. answerItemTemplate
  , _filledQuestionAnswers = question2 ^. answers
  , _filledQuestionAnswerValue = Nothing
  , _filledQuestionAnswerOption = Just fQ2_answerYes
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = question2 ^. experts
  , _filledQuestionReferences = question2 ^. references
  }

fQuestion3 =
  FilledQuestion
  { _filledQuestionUuid = question3 ^. uuid
  , _filledQuestionQType = question3 ^. qType
  , _filledQuestionTitle = question3 ^. title
  , _filledQuestionText = question3 ^. text
  , _filledQuestionRequiredLevel = question3 ^. requiredLevel
  , _filledQuestionTagUuids = question3 ^. tagUuids
  , _filledQuestionAnswerItemTemplate = question3 ^. answerItemTemplate
  , _filledQuestionAnswers = question3 ^. answers
  , _filledQuestionAnswerValue = Nothing
  , _filledQuestionAnswerOption = Just fQ3_answerNo
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = question3 ^. experts
  , _filledQuestionReferences = question3 ^. references
  }

fQuestion4 =
  FilledQuestion
  { _filledQuestionUuid = question4 ^. uuid
  , _filledQuestionQType = question4 ^. qType
  , _filledQuestionTitle = question4 ^. title
  , _filledQuestionText = question4 ^. text
  , _filledQuestionRequiredLevel = question4 ^. requiredLevel
  , _filledQuestionTagUuids = question4 ^. tagUuids
  , _filledQuestionAnswerItemTemplate = question4 ^. answerItemTemplate
  , _filledQuestionAnswers = question4 ^. answers
  , _filledQuestionAnswerValue = Nothing
  , _filledQuestionAnswerOption = Nothing
  , _filledQuestionAnswerItems = Just [fQ4_ait1, fQ4_ait2]
  , _filledQuestionExperts = question4 ^. experts
  , _filledQuestionReferences = question4 ^. references
  }
