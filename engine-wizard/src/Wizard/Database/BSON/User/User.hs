module Wizard.Database.BSON.User.User where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Wizard.Database.BSON.Common ()
import Wizard.Model.User.User

instance ToBSON User where
  toBSON User {..} =
    [ "uuid" BSON.=: _userUuid
    , "firstName" BSON.=: _userFirstName
    , "lastName" BSON.=: _userLastName
    , "email" BSON.=: _userEmail
    , "passwordHash" BSON.=: _userPasswordHash
    , "affiliation" BSON.=: _userAffiliation
    , "sources" BSON.=: _userSources
    , "role" BSON.=: _userRole
    , "permissions" BSON.=: _userPermissions
    , "active" BSON.=: _userActive
    , "createdAt" BSON.=: _userCreatedAt
    , "updatedAt" BSON.=: _userUpdatedAt
    ]

instance FromBSON User where
  fromBSON doc = do
    _userUuid <- BSON.lookup "uuid" doc
    _userFirstName <- BSON.lookup "firstName" doc
    _userLastName <- BSON.lookup "lastName" doc
    _userEmail <- BSON.lookup "email" doc
    _userPasswordHash <- BSON.lookup "passwordHash" doc
    _userAffiliation <- BSON.lookup "affiliation" doc
    _userSources <- BSON.lookup "sources" doc
    _userRole <- BSON.lookup "role" doc
    _userPermissions <- BSON.lookup "permissions" doc
    _userActive <- BSON.lookup "active" doc
    _userCreatedAt <- BSON.lookup "createdAt" doc
    _userUpdatedAt <- BSON.lookup "updatedAt" doc
    return User {..}
