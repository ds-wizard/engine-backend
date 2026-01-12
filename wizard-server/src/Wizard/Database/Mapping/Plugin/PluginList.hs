module Wizard.Database.Mapping.Plugin.PluginList where

import Database.PostgreSQL.Simple

import Wizard.Model.Plugin.PluginList

instance ToRow PluginList

instance FromRow PluginList
