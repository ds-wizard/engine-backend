module Wizard.Api.Resource.User.UserProfileDTO where

import Data.Time
import Data.UUID
import GHC.Generics

import Wizard.Api.Resource.User.UserSubmissionPropsDTO

data UserProfileDTO =
  UserProfileDTO
    { _userProfileDTOUuid :: UUID
    , _userProfileDTOFirstName :: String
    , _userProfileDTOLastName :: String
    , _userProfileDTOEmail :: String
    , _userProfileDTOAffiliation :: Maybe String
    , _userProfileDTOSources :: [String]
    , _userProfileDTORole :: String
    , _userProfileDTOPermissions :: [String]
    , _userProfileDTOActive :: Bool
    , _userProfileDTOSubmissionProps :: [UserSubmissionPropsDTO]
    , _userProfileDTOImageUrl :: Maybe String
    , _userProfileDTOCreatedAt :: UTCTime
    , _userProfileDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq UserProfileDTO where
  a == b =
    _userProfileDTOUuid a == _userProfileDTOUuid b &&
    _userProfileDTOFirstName a == _userProfileDTOFirstName b &&
    _userProfileDTOLastName a == _userProfileDTOLastName b &&
    _userProfileDTOEmail a == _userProfileDTOEmail b &&
    _userProfileDTOAffiliation a == _userProfileDTOAffiliation b &&
    _userProfileDTOSources a == _userProfileDTOSources b &&
    _userProfileDTORole a == _userProfileDTORole b &&
    _userProfileDTOPermissions a == _userProfileDTOPermissions b &&
    _userProfileDTOActive a == _userProfileDTOActive b &&
    _userProfileDTOSubmissionProps a == _userProfileDTOSubmissionProps b &&
    _userProfileDTOImageUrl a == _userProfileDTOImageUrl b
