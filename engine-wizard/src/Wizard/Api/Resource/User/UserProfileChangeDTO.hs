module Wizard.Api.Resource.User.UserProfileChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Model.User.User

data UserProfileChangeDTO =
  UserProfileChangeDTO
    { _userProfileChangeDTOFirstName :: String
    , _userProfileChangeDTOLastName :: String
    , _userProfileChangeDTOEmail :: Email
    , _userProfileChangeDTOAffiliation :: Maybe String
    , _userProfileChangeDTOSubmissionProps :: [UserSubmissionPropsDTO]
    }
  deriving (Generic)
