module Wizard.Database.Mapping.User.User where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Wizard.Api.Resource.User.UserSubmissionPropJM ()
import Wizard.Model.User.User

instance ToRow User where
  toRow User {..} =
    [ toField uuid
    , toField firstName
    , toField lastName
    , toField email
    , toField passwordHash
    , toField affiliation
    , toField role.uuid
    , toField . PGArray $ role.permissions
    , toField active
    , toField imageUrl
    , toField lastVisitedAt
    , toField createdAt
    , toField updatedAt
    , toField tenantUuid
    , toField machine
    , toField locale
    , toField lastSeenNewsId
    , toField emailVerifiedAt
    , toField emailPending
    , toField role.name
    ]

instance FromRow User where
  fromRow = do
    uuid <- field
    firstName <- field
    lastName <- field
    email <- field
    passwordHash <- field
    affiliation <- field
    roleUuid <- field
    rolePermissions <- fromPGArray <$> field
    active <- field
    imageUrl <- field
    lastVisitedAt <- field
    createdAt <- field
    updatedAt <- field
    tenantUuid <- field
    machine <- field
    locale <- field
    lastSeenNewsId <- field
    emailVerifiedAt <- field
    emailPending <- field
    roleName <- field
    let role = RoleSimple {uuid = roleUuid, name = roleName, permissions = rolePermissions}
    return $ User {..}
