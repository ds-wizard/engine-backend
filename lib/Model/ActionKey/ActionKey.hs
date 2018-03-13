module Model.ActionKey.ActionKey where

import Control.Lens (makeLenses)
import Data.UUID
import GHC.Generics

import Common.Types

data ActionKeyType
  = RegistrationActionKey
  | ForgottenPasswordActionKey
  deriving (Generic, Show, Eq)

data ActionKey = ActionKey
  { _akUuid :: UUID
  , _akUserId :: UUID
  , _akType :: ActionKeyType
  , _akHash :: String
  } deriving (Generic, Show, Eq)

makeLenses ''ActionKey
