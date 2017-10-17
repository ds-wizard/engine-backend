module Database.Entity.User where

import Control.Lens (makeLenses, (^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.UUID
import Data.Maybe
import GHC.Generics

import Common.Types
import Common.Uuid

data User = User
  { _uUuid :: UUID
  , _uName :: String
  , _uSurname :: String
  , _uRole :: Role
  , _uPermissions :: [Permission]
  } deriving (Generic, Show, Eq)

makeLenses ''User

instance ToBSON User where
  toBSON user =
    [ "uuid" BSON.=: toString (user ^. uUuid)
    , "name" BSON.=: (user ^. uName)
    , "surname" BSON.=: (user ^. uSurname)
    , "role" BSON.=: (user ^. uRole)
    , "permissions" BSON.=: (user ^. uPermissions)
    ]

instance FromBSON User where
  fromBSON doc = do
    uuidS <- BSON.lookup "uuid" doc
    uuid <- fromString uuidS
    name <- BSON.lookup "name" doc
    surname <- BSON.lookup "surname" doc
    role <- BSON.lookup "role" doc
    permissions <- BSON.lookup "permissions" doc
    return User
      { _uUuid = uuid
      , _uName = name
      , _uSurname = surname
      , _uRole = role
      , _uPermissions = permissions
      }


