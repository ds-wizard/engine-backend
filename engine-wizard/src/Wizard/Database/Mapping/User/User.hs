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
    [ toField _userUuid
    , toField _userFirstName
    , toField _userLastName
    , toField _userEmail
    , toField _userPasswordHash
    , toField _userAffiliation
    , toJSONField _userSources
    , toField _userRole
    , toField . PGArray $ _userPermissions
    , toField _userActive
    , toJSONField _userSubmissionProps
    , toField _userImageUrl
    , toJSONField _userGroups
    , toField _userLastVisitedAt
    , toField _userCreatedAt
    , toField _userUpdatedAt
    , toField _userAppUuid
    , toField _userMachine
    ]

instance FromRow User where
  fromRow = do
    _userUuid <- field
    _userFirstName <- field
    _userLastName <- field
    _userEmail <- field
    _userPasswordHash <- field
    _userAffiliation <- field
    _userSources <- fieldWith fromJSONField
    _userRole <- field
    _userPermissions <- fromPGArray <$> field
    _userActive <- field
    _userSubmissionProps <- fieldWith fromJSONField
    _userImageUrl <- field
    _userGroups <- fieldWith fromJSONField
    _userLastVisitedAt <- field
    _userCreatedAt <- field
    _userUpdatedAt <- field
    _userAppUuid <- field
    _userMachine <- field
    return $ User {..}
