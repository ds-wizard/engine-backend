module Wizard.Model.User.UserSubmissionPropList where

import qualified Data.Map.Strict as M
import GHC.Generics

data UserSubmissionPropList = UserSubmissionPropList
  { sId :: String
  , name :: String
  , values :: M.Map String String
  }
  deriving (Generic, Eq, Show)
