module Model.ActionKey.ActionKey where

import Data.UUID
import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenPasswordActionKey
  deriving (Generic, Show, Eq)

data ActionKey = ActionKey
  { _actionKeyUuid :: UUID
  , _actionKeyUserId :: UUID
  , _actionKeyAType :: ActionKeyType
  , _actionKeyHash :: String
  } deriving (Generic, Show, Eq)
