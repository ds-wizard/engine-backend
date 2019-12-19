module Wizard.Model.ActionKey.ActionKey where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenPasswordActionKey
  deriving (Show, Eq, Generic)

data ActionKey =
  ActionKey
    { _actionKeyUuid :: U.UUID
    , _actionKeyUserId :: U.UUID
    , _actionKeyAType :: ActionKeyType
    , _actionKeyHash :: String
    , _actionKeyCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq ActionKey where
  a == b =
    _actionKeyUuid a == _actionKeyUuid b &&
    _actionKeyUserId a == _actionKeyUserId b &&
    _actionKeyAType a == _actionKeyAType b && _actionKeyHash a == _actionKeyHash b
