module Wizard.Model.User.UserToken where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserToken = UserToken
  { uuid :: U.UUID
  , userUuid :: U.UUID
  , value :: String
  , sessionState :: Maybe String
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq UserToken where
  a == b =
    uuid a == uuid b
      && userUuid a == userUuid b
      && value a == value b
      && sessionState a == sessionState b
      && appUuid a == appUuid b
