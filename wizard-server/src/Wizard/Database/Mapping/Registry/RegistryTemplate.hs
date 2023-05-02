module Wizard.Database.Mapping.Registry.RegistryTemplate where

import Database.PostgreSQL.Simple

import Wizard.Model.Registry.RegistryTemplate

instance ToRow RegistryTemplate

instance FromRow RegistryTemplate
