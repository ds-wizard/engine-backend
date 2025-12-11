module Wizard.Database.Mapping.Project.ProjectSuggestion where

import Database.PostgreSQL.Simple

import Wizard.Model.Project.ProjectSuggestion

instance FromRow ProjectSuggestion
