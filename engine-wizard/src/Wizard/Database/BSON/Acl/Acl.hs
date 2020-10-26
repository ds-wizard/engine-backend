module Wizard.Database.BSON.Acl.Acl where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Acl.Acl

instance ToBSON Group

instance FromBSON Group

instance BSON.Val GroupMembershipType where
  val = genericVal
  cast' = genericCast'

instance ToBSON GroupMembership

instance FromBSON GroupMembership

instance ToBSON Member where
  toBSON GroupMember {..} = ["type" BSON.=: "GroupMember", "id" BSON.=: _groupMemberGId]
  toBSON UserMember {..} = ["type" BSON.=: "UserMember", "uuid" BSON.=: _userMemberUuid]

instance FromBSON Member where
  fromBSON doc = do
    eType <- BSON.lookup "type" doc
    case eType of
      "GroupMember" -> do
        _groupMemberGId <- BSON.lookup "id" doc
        return GroupMember {..}
      "UserMember" -> do
        _userMemberUuid <- BSON.lookup "uuid" doc
        return UserMember {..}
