module Database.BSON.ActionKey.ActionKey where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.ActionKey.ActionKey

instance ToBSON ActionKey where
  toBSON actionKey =
    [ "uuid" BSON.=: toString (actionKey ^. akUuid)
    , "userId" BSON.=: toString (actionKey ^. akUserId)
    , "type" BSON.=: show (actionKey ^. akType)
    , "hash" BSON.=: (actionKey ^. akHash)
    ]

--    , "createdAt" BSON.=: (actionKey ^. akCreatedAt)
instance FromBSON ActionKey where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    userId <- deserializeUUID $ BSON.lookup "userId" doc
    actionType <- deserializeActionType $ BSON.lookup "type" doc
    hash <- BSON.lookup "hash" doc
    return ActionKey {_akUuid = uuid, _akUserId = userId, _akType = actionType, _akHash = hash}
    where
      deserializeActionType :: Maybe String -> Maybe ActionKeyType
      deserializeActionType mActionTypeS = do
        actionType <- mActionTypeS
        case actionType of
          "RegistrationActionKey" -> Just RegistrationActionKey
          "ForgottenPasswordActionKey" -> Just ForgottenPasswordActionKey
