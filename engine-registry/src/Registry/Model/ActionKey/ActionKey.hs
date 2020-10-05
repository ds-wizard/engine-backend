module Registry.Model.ActionKey.ActionKey where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenTokenActionKey
  deriving (Show, Eq, Generic, Read)

data ActionKey =
  ActionKey
    { _actionKeyUuid :: U.UUID
    , _actionKeyOrganizationId :: String
    , _actionKeyAType :: ActionKeyType
    , _actionKeyHash :: String
    , _actionKeyCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ActionKey where
  a == b =
    _actionKeyUuid a == _actionKeyUuid b &&
    _actionKeyOrganizationId a == _actionKeyOrganizationId b &&
    _actionKeyAType a == _actionKeyAType b && _actionKeyHash a == _actionKeyHash b
