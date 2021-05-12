module Wizard.Database.Mapping.Level.Level where

import Database.PostgreSQL.Simple

import Wizard.Model.Level.Level

instance ToRow Level

instance FromRow Level
