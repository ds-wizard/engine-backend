module Wizard.Api.Resource.Acl.MemberDTO where

import qualified Data.UUID as U
import GHC.Generics

data MemberDTO
  = UserMemberDTO
      { uuid :: U.UUID
      , firstName :: String
      , lastName :: String
      , gravatarHash :: String
      , imageUrl :: Maybe String
      }
  | UserGroupMemberDTO
      { uuid :: U.UUID
      , name :: String
      , description :: Maybe String
      , private :: Bool
      }
  deriving (Generic, Eq, Show)
