module Wizard.Model.Questionnaire.QuestionnaireContentDM where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.Phases
import Wizard.Model.Questionnaire.QuestionnaireContent

defaultQuestionnaireContent :: QuestionnaireContent
defaultQuestionnaireContent =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid = phase1 ^. uuid
    , _questionnaireContentReplies = M.empty
    , _questionnaireContentLabels = M.empty
    }
