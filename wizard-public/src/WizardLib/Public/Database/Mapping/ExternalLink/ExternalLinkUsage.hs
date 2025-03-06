module WizardLib.Public.Database.Mapping.ExternalLink.ExternalLinkUsage where

import Database.PostgreSQL.Simple

import WizardLib.Public.Model.ExternalLink.ExternalLinkUsage

instance ToRow ExternalLinkUsage

instance FromRow ExternalLinkUsage
