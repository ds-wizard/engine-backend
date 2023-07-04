module Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommand where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Common.Database.Mapping.Common
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

instance ToField PersistentCommandState where
  toField = toFieldGenericEnum

instance FromField PersistentCommandState where
  fromField = fromFieldGenericEnum

instance FromField identity => FromRow (PersistentCommand identity) where
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
    destination <- field
    return $ PersistentCommand {..}

instance ToField identity => ToRow (PersistentCommand identity) where
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
    , toField destination
    ]
