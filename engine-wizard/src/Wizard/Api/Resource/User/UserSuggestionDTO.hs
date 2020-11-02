module Wizard.Api.Resource.User.UserSuggestionDTO where

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
  deriving (Generic)
