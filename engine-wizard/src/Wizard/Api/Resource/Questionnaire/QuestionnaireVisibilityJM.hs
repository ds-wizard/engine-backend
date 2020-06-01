module Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM where

import Data.Aeson

import Wizard.Model.Questionnaire.Questionnaire

instance FromJSON QuestionnaireVisibility

instance ToJSON QuestionnaireVisibility
