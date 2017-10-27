module Database.BSON.User.User where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Model.User

instance ToBSON User where
  toBSON user =
    [ "uuid" BSON.=: toString (user ^. uUuid)
    , "name" BSON.=: (user ^. uName)
    , "surname" BSON.=: (user ^. uSurname)
    , "email" BSON.=: (user ^. uEmail)
    , "passwordHash" BSON.=: (user ^. uPasswordHash)
    , "role" BSON.=: (user ^. uRole)
    , "permissions" BSON.=: (user ^. uPermissions)
    ]

instance FromBSON User where
  fromBSON doc = do
    uuidS <- BSON.lookup "uuid" doc
    uuid <- fromString uuidS
    name <- BSON.lookup "name" doc
    surname <- BSON.lookup "surname" doc
    email <- BSON.lookup "email" doc
    passwordHash <- BSON.lookup "passwordHash" doc
    role <- BSON.lookup "role" doc
    permissions <- BSON.lookup "permissions" doc
    return
      User
      { _uUuid = uuid
      , _uName = name
      , _uSurname = surname
      , _uEmail = email
      , _uPasswordHash = passwordHash
      , _uRole = role
      , _uPermissions = permissions
      }
