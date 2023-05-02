module Wizard.Api.Resource.Acl.MemberDTO where

import qualified Data.UUID as U
import GHC.Generics

data MemberDTO
  = GroupMemberDTO
      { gId :: String
      , name :: String
      }
  | UserMemberDTO
      { uuid :: U.UUID
      , firstName :: String
      , lastName :: String
      , gravatarHash :: String
      , imageUrl :: Maybe String
      }
  deriving (Generic, Eq, Show)
