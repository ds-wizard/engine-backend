module Wizard.Database.Mapping.Registry.RegistryOrganization where

import Database.PostgreSQL.Simple

import Wizard.Model.Registry.RegistryOrganization

instance ToRow RegistryOrganization

instance FromRow RegistryOrganization
