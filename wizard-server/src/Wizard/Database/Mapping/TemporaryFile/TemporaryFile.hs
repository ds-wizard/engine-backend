module Wizard.Database.Mapping.TemporaryFile.TemporaryFile where

import Database.PostgreSQL.Simple

import Shared.Common.Database.Mapping.Common ()
import Wizard.Model.TemporaryFile.TemporaryFile

instance FromRow TemporaryFile

instance ToRow TemporaryFile
