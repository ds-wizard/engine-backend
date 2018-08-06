module Model.FilledKnowledgeModel.FilledKnowledgeModelAccessors where

import Control.Lens

import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel

getAllFilledQuestionsForChapter :: FilledChapter -> [FilledQuestion]
getAllFilledQuestionsForChapter ch = go (ch ^.. questions . traverse)
  where
    go :: [FilledQuestion] -> [FilledQuestion]
    go [] = []
    go questions = questions ++ (go . concat $ getNestedQuestions <$> questions)
    getNestedQuestions :: FilledQuestion -> [FilledQuestion]
    getNestedQuestions FilledQuestion {_filledQuestionAnswerOption = (Just ao)} = ao ^. followUps
    getNestedQuestions FilledQuestion {_filledQuestionAnswerItems = (Just ais)} =
      concat $ _filledAnswerItemQuestions <$> ais
    getNestedQuestions _ = []
