module Wizard.Database.Mapping.PersistentCommand.PersistentCommand where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Wizard.Model.PersistentCommand.PersistentCommand

instance ToField PersistentCommandState where
  toField = toFieldGenericEnum

instance FromField PersistentCommandState where
  fromField = fromFieldGenericEnum

instance ToRow PersistentCommand

instance FromRow PersistentCommand
