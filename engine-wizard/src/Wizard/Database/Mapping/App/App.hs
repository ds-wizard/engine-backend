module Wizard.Database.Mapping.App.App where

import Database.PostgreSQL.Simple

import Wizard.Model.App.App

instance ToRow App

instance FromRow App
