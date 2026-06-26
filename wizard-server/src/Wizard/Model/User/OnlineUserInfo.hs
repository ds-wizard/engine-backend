module Wizard.Model.User.OnlineUserInfo (
  OnlineUserInfo (..),
  module WizardLib.Public.Model.User.RoleSimple,
) where

import qualified Data.UUID as U
import GHC.Generics

import WizardLib.Public.Model.User.RoleSimple

data OnlineUserInfo
  = LoggedOnlineUserInfo
      { uuid :: U.UUID
      , firstName :: String
      , lastName :: String
      , gravatarHash :: String
      , imageUrl :: Maybe String
      , colorNumber :: Int
      , role :: RoleSimple
      , groupUuids :: [U.UUID]
      }
  | AnonymousOnlineUserInfo
      { avatarNumber :: Int
      , colorNumber :: Int
      }
  deriving (Show, Eq, Generic)
