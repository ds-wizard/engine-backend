module Wizard.Model.ActionKey.ActionKey where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data ActionKeyType
  = RegistrationActionKey
  | ForgottenPasswordActionKey
  deriving (Show, Eq, Generic, Read)

data ActionKey = ActionKey
  { uuid :: U.UUID
  , userId :: U.UUID
  , aType :: ActionKeyType
  , hash :: String
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ActionKey where
  a == b =
    uuid a == uuid b
      && userId a == userId b
      && aType a == aType b
      && hash a == hash b
      && appUuid a == appUuid b
