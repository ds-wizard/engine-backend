module WizardLib.Public.Model.User.UserSuggestion where

import Data.Hashable
import qualified Data.UUID as U
import GHC.Generics

data UserSuggestion = UserSuggestion
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , gravatarHash :: String
  , imageUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance Hashable UserSuggestion
