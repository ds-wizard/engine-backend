module Wizard.Model.User.UserSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data UserSuggestion = UserSuggestion
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , imageUrl :: Maybe String
  }
  deriving (Generic, Eq, Show)
