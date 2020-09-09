module Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM where

import Data.Aeson

import Wizard.Model.Questionnaire.Questionnaire

instance FromJSON QuestionnaireSharing

instance ToJSON QuestionnaireSharing
