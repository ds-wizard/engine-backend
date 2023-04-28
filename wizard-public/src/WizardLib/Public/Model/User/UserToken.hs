module WizardLib.Public.Model.User.UserToken where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserTokenType
  = LoginUserTokenType
  | ApiKeyUserTokenType
  deriving (Show, Eq, Generic, Read)

data UserToken = UserToken
  { uuid :: U.UUID
  , name :: String
  , tType :: UserTokenType
  , userUuid :: U.UUID
  , value :: String
  , userAgent :: String
  , sessionState :: Maybe String
  , expiresAt :: UTCTime
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq UserToken where
  a == b =
    uuid a == uuid b
      && name a == name b
      && tType a == tType b
      && userUuid a == userUuid b
      && value a == value b
      && userAgent a == userAgent b
      && sessionState a == sessionState b
      && expiresAt a == expiresAt b
      && appUuid a == appUuid b
