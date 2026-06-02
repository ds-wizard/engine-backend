module Shared.UserEmailLink.Api.Resource.UserEmailLink.UserEmailLinkDTO where

import GHC.Generics

data UserEmailLinkDTO aType = UserEmailLinkDTO
  { aType :: aType
  , email :: String
  }
  deriving (Show, Eq, Generic)
