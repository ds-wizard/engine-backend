module Wizard.Model.User.UserTokenList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserTokenList = UserTokenList
  { uuid :: U.UUID
  , name :: String
  , userAgent :: String
  , currentSession :: Bool
  , expiresAt :: UTCTime
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq UserTokenList where
  a == b =
    uuid a == uuid b
      && name a == name b
      && userAgent a == userAgent b
      && currentSession a == currentSession b
      && expiresAt a == expiresAt b
