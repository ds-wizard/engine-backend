module Wizard.Api.Resource.User.UserSubmissionPropsDTO where

import qualified Data.Map.Strict as M
import GHC.Generics

data UserSubmissionPropsDTO = UserSubmissionPropsDTO
  { sId :: String
  , name :: String
  , values :: M.Map String String
  }
  deriving (Generic, Eq, Show)

instance Ord UserSubmissionPropsDTO where
  compare a b = compare (sId a) (sId b)
