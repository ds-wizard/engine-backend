module WizardLib.Public.Model.User.UserSimple where

import qualified Data.UUID as U
import GHC.Generics

data UserSimple = UserSimple
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , imageUrl :: Maybe String
  }
  deriving (Generic, Eq, Show)
