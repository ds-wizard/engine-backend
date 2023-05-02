module Wizard.Model.Questionnaire.QuestionnaireContentDM where

import qualified Data.Map.Strict as M

import Wizard.Model.Questionnaire.QuestionnaireContent

defaultQuestionnaireContent :: QuestionnaireContent
defaultQuestionnaireContent =
  QuestionnaireContent
    { phaseUuid = Nothing
    , replies = M.empty
    , labels = M.empty
    }
