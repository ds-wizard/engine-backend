module WizardLib.Public.Database.Mapping.TemporaryFile.TemporaryFile where

import Database.PostgreSQL.Simple

import Shared.Common.Database.Mapping.Common ()
import WizardLib.Public.Model.TemporaryFile.TemporaryFile

instance FromRow TemporaryFile

instance ToRow TemporaryFile
