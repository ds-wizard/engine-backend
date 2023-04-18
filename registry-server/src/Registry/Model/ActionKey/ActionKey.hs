module Registry.Model.ActionKey.ActionKey where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenTokenActionKey
  deriving (Show, Eq, Generic, Read)

data ActionKey = ActionKey
  { uuid :: U.UUID
  , organizationId :: String
  , aType :: ActionKeyType
  , hash :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ActionKey where
  a == b =
    a.uuid == b.uuid
      && a.organizationId == b.organizationId
      && a.aType == b.aType
      && a.hash == b.hash
