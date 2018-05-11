module Database.BSON.ActionKey.ActionKey where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.UUID

import Database.BSON.Common
import LensesConfig
import Model.ActionKey.ActionKey

instance ToBSON ActionKey where
  toBSON actionKey =
    [ "uuid" BSON.=: toString (actionKey ^. uuid)
    , "userId" BSON.=: toString (actionKey ^. userId)
    , "type" BSON.=: show (actionKey ^. aType)
    , "hash" BSON.=: (actionKey ^. hash)
    ]

instance FromBSON ActionKey where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    userId <- deserializeUUID $ BSON.lookup "userId" doc
    actionType <- deserializeActionType $ BSON.lookup "type" doc
    hash <- BSON.lookup "hash" doc
    return
      ActionKey {_actionKeyUuid = uuid, _actionKeyUserId = userId, _actionKeyAType = actionType, _actionKeyHash = hash}
    where
      deserializeActionType :: Maybe String -> Maybe ActionKeyType
      deserializeActionType mActionTypeS = do
        actionType <- mActionTypeS
        case actionType of
          "RegistrationActionKey" -> Just RegistrationActionKey
          "ForgottenPasswordActionKey" -> Just ForgottenPasswordActionKey
