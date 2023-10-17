module Wizard.Model.User.OnlineUserInfo where

import qualified Data.UUID as U
import GHC.Generics

data OnlineUserInfo
  = LoggedOnlineUserInfo
      { uuid :: U.UUID
      , firstName :: String
      , lastName :: String
      , gravatarHash :: String
      , imageUrl :: Maybe String
      , colorNumber :: Int
      , role :: String
      , groupUuids :: [U.UUID]
      }
  | AnonymousOnlineUserInfo
      { avatarNumber :: Int
      , colorNumber :: Int
      }
  deriving (Show, Eq, Generic)
