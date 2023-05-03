module Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommandSimple where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple

instance FromField identity => FromRow (PersistentCommandSimple identity)

instance ToField identity => ToRow (PersistentCommandSimple identity)
