module Database.BSON.Organization.Organization where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Organization.Organization

instance ToBSON Organization where
  toBSON organization =
    [ "uuid" BSON.=: serializeUUID (organization ^. orgUuid)
    , "name" BSON.=: (organization ^. orgName)
    , "groupId" BSON.=: (organization ^. orgGroupId)
    ]

instance FromBSON Organization where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    groupId <- BSON.lookup "groupId" doc
    return Organization {_orgUuid = uuid, _orgName = name, _orgGroupId = groupId}
