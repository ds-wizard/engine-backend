module Wizard.Model.User.OnlineUserInfo where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Acl.Acl

data OnlineUserInfo
  = LoggedOnlineUserInfo
      { _loggedOnlineUserInfoUuid :: U.UUID
      , _loggedOnlineUserInfoFirstName :: String
      , _loggedOnlineUserInfoLastName :: String
      , _loggedOnlineUserInfoGravatarHash :: String
      , _loggedOnlineUserInfoImageUrl :: Maybe String
      , _loggedOnlineUserInfoColorNumber :: Int
      , _loggedOnlineUserInfoRole :: String
      , _loggedOnlineUserInfoGroups :: [GroupMembership]
      }
  | AnonymousOnlineUserInfo
      { _anonymousOnlineUserInfoAvatarNumber :: Int
      , _anonymousOnlineUserInfoColorNumber :: Int
      }
  deriving (Show, Eq, Generic)
