module Model.FilledKnowledgeModel.FilledKnowledgeModelAccessors where

import Control.Lens
import qualified Data.UUID as U

import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

getAllFilledQuestionsForChapter :: FilledChapter -> [FilledQuestion]
getAllFilledQuestionsForChapter ch = go (ch ^.. questions . traverse)
  where
    go :: [FilledQuestion] -> [FilledQuestion]
    go [] = []
    go questions = questions ++ (go . concat $ getNestedQuestions <$> questions)
    getNestedQuestions :: FilledQuestion -> [FilledQuestion]
    getNestedQuestions (FilledOptionsQuestion' q) =
      case q ^. answerOption of
        Just ao -> ao ^. followUps
        Nothing -> []
    getNestedQuestions (FilledListQuestion' q) =
      case q ^. items of
        Just is -> concat $ _filledAnswerItemQuestions <$> is
        Nothing -> []
    getNestedQuestions _ = []

getFilledQuestionUuid :: FilledQuestion -> U.UUID
getFilledQuestionUuid (FilledOptionsQuestion' q) = q ^. uuid
getFilledQuestionUuid (FilledListQuestion' q) = q ^. uuid
getFilledQuestionUuid (FilledValueQuestion' q) = q ^. uuid
getFilledQuestionUuid (FilledIntegrationQuestion' q) = q ^. uuid

getRequiredLevel :: FilledQuestion -> Maybe Int
getRequiredLevel (FilledOptionsQuestion' q) = q ^. requiredLevel
getRequiredLevel (FilledListQuestion' q) = q ^. requiredLevel
getRequiredLevel (FilledValueQuestion' q) = q ^. requiredLevel
getRequiredLevel (FilledIntegrationQuestion' q) = q ^. requiredLevel
