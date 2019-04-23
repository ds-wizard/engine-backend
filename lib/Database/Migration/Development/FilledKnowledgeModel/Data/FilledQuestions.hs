module Database.Migration.Development.FilledKnowledgeModel.Data.FilledQuestions where

import Control.Lens ((^.))

import Database.Migration.Development.FilledKnowledgeModel.Data.FilledAnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Questions
import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

fQuestion1' :: FilledQuestion
fQuestion1' = FilledValueQuestion' fQuestion1

fQuestion1 :: FilledValueQuestion
fQuestion1 =
  FilledValueQuestion
  { _filledValueQuestionUuid = question1 ^. uuid
  , _filledValueQuestionHumanIdentifier = "1"
  , _filledValueQuestionTitle = question1 ^. title
  , _filledValueQuestionText = question1 ^. text
  , _filledValueQuestionRequiredLevel = question1 ^. requiredLevel
  , _filledValueQuestionTagUuids = question1 ^. tagUuids
  , _filledValueQuestionExperts = question1 ^. experts
  , _filledValueQuestionReferences = question1 ^. references
  , _filledValueQuestionValueType = question1 ^. valueType
  , _filledValueQuestionAnswerValue = Just "Reply to 1st question"
  }

fQuestion2' :: FilledQuestion
fQuestion2' = FilledOptionsQuestion' fQuestion2

fQuestion2 :: FilledOptionsQuestion
fQuestion2 =
  FilledOptionsQuestion
  { _filledOptionsQuestionUuid = question2 ^. uuid
  , _filledOptionsQuestionHumanIdentifier = "2"
  , _filledOptionsQuestionTitle = question2 ^. title
  , _filledOptionsQuestionText = question2 ^. text
  , _filledOptionsQuestionRequiredLevel = question2 ^. requiredLevel
  , _filledOptionsQuestionTagUuids = question2 ^. tagUuids
  , _filledOptionsQuestionExperts = question2 ^. experts
  , _filledOptionsQuestionReferences = question2 ^. references
  , _filledOptionsQuestionAnswers = question2 ^. answers
  , _filledOptionsQuestionAnswerOption = Just fQ2_answerYes
  }

fQuestion3' :: FilledQuestion
fQuestion3' = FilledOptionsQuestion' fQuestion3

fQuestion3 :: FilledOptionsQuestion
fQuestion3 =
  FilledOptionsQuestion
  { _filledOptionsQuestionUuid = question3 ^. uuid
  , _filledOptionsQuestionHumanIdentifier = "1"
  , _filledOptionsQuestionTitle = question3 ^. title
  , _filledOptionsQuestionText = question3 ^. text
  , _filledOptionsQuestionRequiredLevel = question3 ^. requiredLevel
  , _filledOptionsQuestionTagUuids = question3 ^. tagUuids
  , _filledOptionsQuestionExperts = question3 ^. experts
  , _filledOptionsQuestionReferences = question3 ^. references
  , _filledOptionsQuestionAnswers = question3 ^. answers
  , _filledOptionsQuestionAnswerOption = Just fQ3_answerNo
  }

fQuestion4' :: FilledQuestion
fQuestion4' = FilledListQuestion' fQuestion4

fQuestion4 :: FilledListQuestion
fQuestion4 =
  FilledListQuestion
  { _filledListQuestionUuid = question4 ^. uuid
  , _filledListQuestionHumanIdentifier = "2"
  , _filledListQuestionTitle = question4 ^. title
  , _filledListQuestionText = question4 ^. text
  , _filledListQuestionRequiredLevel = question4 ^. requiredLevel
  , _filledListQuestionTagUuids = question4 ^. tagUuids
  , _filledListQuestionExperts = question4 ^. experts
  , _filledListQuestionReferences = question4 ^. references
  , _filledListQuestionItemTemplateTitle = question4 ^. itemTemplateTitle
  , _filledListQuestionItemTemplateQuestions = question4 ^. itemTemplateQuestions
  , _filledListQuestionItems = Just [fQ4_ai1, fQ4_ai2]
  }

fQuestion9' :: FilledQuestion
fQuestion9' = FilledIntegrationQuestion' fQuestion9

fQuestion9 :: FilledIntegrationQuestion
fQuestion9 =
  FilledIntegrationQuestion
  { _filledIntegrationQuestionUuid = question9 ^. uuid
  , _filledIntegrationQuestionHumanIdentifier = "1"
  , _filledIntegrationQuestionTitle = question9 ^. title
  , _filledIntegrationQuestionText = question9 ^. text
  , _filledIntegrationQuestionRequiredLevel = question9 ^. requiredLevel
  , _filledIntegrationQuestionTagUuids = question9 ^. tagUuids
  , _filledIntegrationQuestionExperts = question9 ^. experts
  , _filledIntegrationQuestionReferences = question9 ^. references
  , _filledIntegrationQuestionIntegrationUuid = question9 ^. integrationUuid
  , _filledIntegrationQuestionProps = question9 ^. props
  , _filledIntegrationQuestionAnswerIntId = Nothing
  , _filledIntegrationQuestionAnswerValue = Just "Plain reply to 9st question"
  }

fQuestion10' :: FilledQuestion
fQuestion10' = FilledIntegrationQuestion' fQuestion10

fQuestion10 :: FilledIntegrationQuestion
fQuestion10 =
  FilledIntegrationQuestion
  { _filledIntegrationQuestionUuid = question10 ^. uuid
  , _filledIntegrationQuestionHumanIdentifier = "2"
  , _filledIntegrationQuestionTitle = question10 ^. title
  , _filledIntegrationQuestionText = question10 ^. text
  , _filledIntegrationQuestionRequiredLevel = question10 ^. requiredLevel
  , _filledIntegrationQuestionTagUuids = question10 ^. tagUuids
  , _filledIntegrationQuestionExperts = question10 ^. experts
  , _filledIntegrationQuestionReferences = question10 ^. references
  , _filledIntegrationQuestionIntegrationUuid = question10 ^. integrationUuid
  , _filledIntegrationQuestionProps = question10 ^. props
  , _filledIntegrationQuestionAnswerIntId = Just "bsg-p000007"
  , _filledIntegrationQuestionAnswerValue = Just "Integration reply to 9st question"
  }
