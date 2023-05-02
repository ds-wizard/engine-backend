module Wizard.Database.Mapping.Questionnaire.QuestionnaireComment where

import Database.PostgreSQL.Simple

import Wizard.Model.Questionnaire.QuestionnaireComment

instance ToRow QuestionnaireComment

instance FromRow QuestionnaireComment
