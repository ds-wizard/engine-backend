module WizardLib.Public.Database.Mapping.User.UserGroup where

import Database.PostgreSQL.Simple

import WizardLib.Public.Model.User.UserGroup

instance ToRow UserGroup

instance FromRow UserGroup
