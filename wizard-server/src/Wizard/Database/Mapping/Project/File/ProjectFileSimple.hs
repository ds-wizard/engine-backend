module Wizard.Database.Mapping.Project.File.ProjectFileSimple where

import Database.PostgreSQL.Simple

import Wizard.Model.Project.File.ProjectFileSimple

instance FromRow ProjectFileSimple
