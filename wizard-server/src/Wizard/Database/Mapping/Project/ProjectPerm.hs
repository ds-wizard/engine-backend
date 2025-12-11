module Wizard.Database.Mapping.Project.ProjectPerm where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Common.Database.Mapping.Common
import Wizard.Model.Project.Acl.ProjectPerm

instance ToField ProjectPermType where
  toField = toFieldGenericEnum

instance FromField ProjectPermType where
  fromField = fromFieldGenericEnum

instance ToRow ProjectPerm where
  toRow r@ProjectPerm {..} =
    [ toField projectUuid
    , toField memberUuid
    , toField . PGArray $ perms
    , toField tenantUuid
    ]

instance FromRow ProjectPerm where
  fromRow = do
    projectUuid <- field
    memberType <- field
    memberUuid <- field
    perms <- fromPGArray <$> field
    tenantUuid <- field
    return ProjectPerm {..}
