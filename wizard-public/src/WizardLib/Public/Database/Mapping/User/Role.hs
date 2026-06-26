module WizardLib.Public.Database.Mapping.User.Role where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import WizardLib.Public.Model.User.Role

instance ToRow Role where
  toRow Role {..} =
    [ toField uuid
    , toField name
    , toField . PGArray $ permissions
    , toField isAdmin
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow Role where
  fromRow = do
    uuid <- field
    name <- field
    permissions <- fromPGArray <$> field
    isAdmin <- field
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ Role {..}
