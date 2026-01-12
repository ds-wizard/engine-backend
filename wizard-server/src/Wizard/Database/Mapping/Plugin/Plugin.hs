module Wizard.Database.Mapping.Plugin.Plugin where

import Database.PostgreSQL.Simple

import Wizard.Model.Plugin.Plugin

instance ToRow Plugin

instance FromRow Plugin
