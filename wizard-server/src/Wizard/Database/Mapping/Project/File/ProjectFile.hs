module Wizard.Database.Mapping.Project.File.ProjectFile where

import Database.PostgreSQL.Simple

import Wizard.Model.Project.File.ProjectFile

instance ToRow ProjectFile

instance FromRow ProjectFile
