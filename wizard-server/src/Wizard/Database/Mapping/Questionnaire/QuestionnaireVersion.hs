module Wizard.Database.Mapping.Questionnaire.QuestionnaireVersion where

import Database.PostgreSQL.Simple

import Wizard.Model.Questionnaire.QuestionnaireVersion

instance ToRow QuestionnaireVersion

instance FromRow QuestionnaireVersion
