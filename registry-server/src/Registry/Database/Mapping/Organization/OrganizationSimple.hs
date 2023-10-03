module Registry.Database.Mapping.Organization.OrganizationSimple where

import Database.PostgreSQL.Simple

import RegistryLib.Model.Organization.OrganizationSimple

instance FromRow OrganizationSimple
