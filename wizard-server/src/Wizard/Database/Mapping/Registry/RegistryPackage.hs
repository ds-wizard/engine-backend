module Wizard.Database.Mapping.Registry.RegistryPackage where

import Database.PostgreSQL.Simple

import Wizard.Model.Registry.RegistryPackage

instance ToRow RegistryPackage

instance FromRow RegistryPackage
