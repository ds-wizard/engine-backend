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
    uuid <- field
    state <- field
    component <- field
    function <- field
    body <- field
    lastErrorMessage <- field
    attempts <- field
    maxAttempts <- field
    appUuid <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    internal <- field
    return $ PersistentCommand {..}

instance ToRow PersistentCommand where
  toRow PersistentCommand {..} =
    [ toField uuid
    , toField state
    , toField component
    , toField function
    , toField body
    , toField lastErrorMessage
    , toField attempts
    , toField maxAttempts
    , toField appUuid
    , toField createdBy
    , toField createdAt
    , toField updatedAt
    , toField internal
    ]
