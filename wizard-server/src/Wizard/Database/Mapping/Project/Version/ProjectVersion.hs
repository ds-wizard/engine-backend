module Wizard.Database.Mapping.Project.Version.ProjectVersion where

import Database.PostgreSQL.Simple

import Wizard.Model.Project.Version.ProjectVersion

instance ToRow ProjectVersion

instance FromRow ProjectVersion
