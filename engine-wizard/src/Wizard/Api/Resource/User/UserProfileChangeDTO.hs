module Wizard.Api.Resource.User.UserProfileChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.User.UserSubmissionPropsDTO

data UserProfileChangeDTO = UserProfileChangeDTO
  { firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , submissionProps :: [UserSubmissionPropsDTO]
  }
  deriving (Generic)
