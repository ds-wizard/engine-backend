module Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserEmailLink identity aType = UserEmailLink
  { uuid :: U.UUID
  , identity :: identity
  , aType :: aType
  , hash :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance (Eq identity, Eq aType) => Eq (UserEmailLink identity aType) where
  a == b =
    uuid a == uuid b
      && identity a == identity b
      && aType a == aType b
      && hash a == hash b
      && tenantUuid a == tenantUuid b
