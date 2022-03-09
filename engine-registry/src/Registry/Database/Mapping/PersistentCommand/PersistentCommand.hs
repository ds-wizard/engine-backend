module Registry.Database.Mapping.PersistentCommand.PersistentCommand where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Registry.Model.PersistentCommand.PersistentCommand
import Shared.Database.Mapping.Common

instance ToField PersistentCommandState where
  toField = toFieldGenericEnum

instance FromField PersistentCommandState where
  fromField = fromFieldGenericEnum

instance FromRow PersistentCommand where
  fromRow = do
    _persistentCommandUuid <- field
    _persistentCommandState <- field
    _persistentCommandComponent <- field
    _persistentCommandFunction <- field
    _persistentCommandBody <- field
    _persistentCommandLastErrorMessage <- field
    _persistentCommandAttempts <- field
    _persistentCommandMaxAttempts <- field
    _persistentCommandAppUuid <- field
    _persistentCommandCreatedBy <- field
    _persistentCommandCreatedAt <- field
    _persistentCommandUpdatedAt <- field
    _persistentCommandInternal <- field
    return $ PersistentCommand {..}

instance ToRow PersistentCommand where
  toRow PersistentCommand {..} =
    [ toField _persistentCommandUuid
    , toField _persistentCommandState
    , toField _persistentCommandComponent
    , toField _persistentCommandFunction
    , toField _persistentCommandBody
    , toField _persistentCommandLastErrorMessage
    , toField _persistentCommandAttempts
    , toField _persistentCommandMaxAttempts
    , toField _persistentCommandAppUuid
    , toField _persistentCommandCreatedBy
    , toField _persistentCommandCreatedAt
    , toField _persistentCommandUpdatedAt
    , toField _persistentCommandInternal
    ]
