module Wizard.Model.Questionnaire.QuestionnaireContentDM where

import qualified Data.Map.Strict as M

import Wizard.Model.Questionnaire.QuestionnaireContent

defaultQuestionnaireContent :: QuestionnaireContent
defaultQuestionnaireContent =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid = Nothing
    , _questionnaireContentReplies = M.empty
    , _questionnaireContentLabels = M.empty
    }
