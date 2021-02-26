module Wizard.Api.Resource.User.UserSuggestionDTO where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

data UserSuggestionDTO =
  UserSuggestionDTO
    { _userSuggestionDTOUuid :: U.UUID
    , _userSuggestionDTOFirstName :: String
    , _userSuggestionDTOLastName :: String
    , _userSuggestionDTOGravatarHash :: String
    , _userSuggestionDTOImageUrl :: Maybe String
    }
  deriving (Show, Eq, Generic)

instance Hashable UserSuggestionDTO
