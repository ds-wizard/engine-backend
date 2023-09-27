module Wizard.Database.Mapping.User.User where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Wizard.Api.Resource.Acl.AclJM ()
import Wizard.Api.Resource.User.UserSubmissionPropsJM ()
import Wizard.Model.User.User

instance ToRow User where
  toRow User {..} =
    [ toField uuid
    , toField firstName
    , toField lastName
    , toField email
    , toField passwordHash
    , toField affiliation
    , toJSONField sources
    , toField uRole
    , toField . PGArray $ permissions
    , toField active
    , toJSONField submissionProps
    , toField imageUrl
    , toJSONField groups
    , toField lastVisitedAt
    , toField createdAt
    , toField updatedAt
    , toField tenantUuid
    , toField machine
    ]

instance FromRow User where
  fromRow = do
    uuid <- field
    firstName <- field
    lastName <- field
    email <- field
    passwordHash <- field
    affiliation <- field
    sources <- fieldWith fromJSONField
    uRole <- field
    permissions <- fromPGArray <$> field
    active <- field
    submissionProps <- fieldWith fromJSONField
    imageUrl <- field
    groups <- fieldWith fromJSONField
    lastVisitedAt <- field
    createdAt <- field
    updatedAt <- field
    tenantUuid <- field
    machine <- field
    return $ User {..}
