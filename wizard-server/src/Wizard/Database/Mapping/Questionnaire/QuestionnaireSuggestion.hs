module Wizard.Database.Mapping.Questionnaire.QuestionnaireSuggestion where

import Database.PostgreSQL.Simple

import Wizard.Model.Questionnaire.QuestionnaireSuggestion

instance FromRow QuestionnaireSuggestion
