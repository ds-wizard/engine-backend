module Wizard.Database.Mapping.Registry.RegistryLocale where

import Database.PostgreSQL.Simple

import Wizard.Model.Registry.RegistryLocale

instance ToRow RegistryLocale

instance FromRow RegistryLocale
