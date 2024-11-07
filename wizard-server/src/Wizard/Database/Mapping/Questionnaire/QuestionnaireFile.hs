module Wizard.Database.Mapping.Questionnaire.QuestionnaireFile where

import Database.PostgreSQL.Simple

import Wizard.Model.Questionnaire.QuestionnaireFile

instance ToRow QuestionnaireFile

instance FromRow QuestionnaireFile
