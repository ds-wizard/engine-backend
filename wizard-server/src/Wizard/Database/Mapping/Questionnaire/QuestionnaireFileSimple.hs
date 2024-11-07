module Wizard.Database.Mapping.Questionnaire.QuestionnaireFileSimple where

import Database.PostgreSQL.Simple

import Wizard.Model.Questionnaire.QuestionnaireFileSimple

instance FromRow QuestionnaireFileSimple
