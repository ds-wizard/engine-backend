module Wizard.Api.Resource.User.UserSubmissionPropsDTO where

import qualified Data.Map.Strict as M
import GHC.Generics

data UserSubmissionPropsDTO =
  UserSubmissionPropsDTO
    { _userSubmissionPropsDTOSId :: String
    , _userSubmissionPropsDTOName :: String
    , _userSubmissionPropsDTOValues :: M.Map String String
    }
  deriving (Generic, Eq, Show)

instance Ord UserSubmissionPropsDTO where
  compare a b = compare (_userSubmissionPropsDTOSId a) (_userSubmissionPropsDTOSId b)
