module Wizard.Api.Resource.Acl.MemberDTO where

import qualified Data.UUID as U
import GHC.Generics

data MemberDTO
  = GroupMemberDTO
      { _groupMemberDTOGId :: String
      , _groupMemberDTOName :: String
      }
  | UserMemberDTO
      { _userMemberDTOUuid :: U.UUID
      , _userMemberDTOFirstName :: String
      , _userMemberDTOLastName :: String
      , _userMemberDTOGravatarHash :: String
      , _userMemberDTOImageUrl :: Maybe String
      }
  deriving (Generic, Eq, Show)
