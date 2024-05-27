module WizardLib.Public.Api.Resource.User.UserSuggestionDTO where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

data UserSuggestionDTO = UserSuggestionDTO
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , gravatarHash :: String
  , imageUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance Hashable UserSuggestionDTO
