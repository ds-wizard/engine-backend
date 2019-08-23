module Database.BSON.User.User where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import LensesConfig
import Model.User.User

instance ToBSON User where
  toBSON user =
    [ "uuid" BSON.=: (user ^. uuid)
    , "name" BSON.=: (user ^. name)
    , "surname" BSON.=: (user ^. surname)
    , "email" BSON.=: (user ^. email)
    , "passwordHash" BSON.=: (user ^. passwordHash)
    , "role" BSON.=: (user ^. role)
    , "permissions" BSON.=: (user ^. permissions)
    , "active" BSON.=: (user ^. active)
    , "createdAt" BSON.=: (user ^. createdAt)
    , "updatedAt" BSON.=: (user ^. updatedAt)
    ]

instance FromBSON User where
  fromBSON doc = do
    uUuid <- BSON.lookup "uuid" doc
    uName <- BSON.lookup "name" doc
    uSurname <- BSON.lookup "surname" doc
    uEmail <- BSON.lookup "email" doc
    uPasswordHash <- BSON.lookup "passwordHash" doc
    uRole <- BSON.lookup "role" doc
    uPermissions <- BSON.lookup "permissions" doc
    uActive <- BSON.lookup "active" doc
    uCreatedAt <- BSON.lookup "createdAt" doc
    uUpdatedAt <- BSON.lookup "updatedAt" doc
    return
      User
      { _userUuid = uUuid
      , _userName = uName
      , _userSurname = uSurname
      , _userEmail = uEmail
      , _userPasswordHash = uPasswordHash
      , _userRole = uRole
      , _userPermissions = uPermissions
      , _userActive = uActive
      , _userCreatedAt = uCreatedAt
      , _userUpdatedAt = uUpdatedAt
      }
