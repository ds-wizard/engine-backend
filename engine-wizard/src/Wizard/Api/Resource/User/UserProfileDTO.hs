module Wizard.Api.Resource.User.UserProfileDTO where

import Data.Time
import Data.UUID
import GHC.Generics

import Wizard.Api.Resource.User.UserSubmissionPropsDTO

data UserProfileDTO = UserProfileDTO
  { uuid :: UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , sources :: [String]
  , uRole :: String
  , permissions :: [String]
  , active :: Bool
  , submissionProps :: [UserSubmissionPropsDTO]
  , imageUrl :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq UserProfileDTO where
  a == b =
    uuid a == uuid b
      && firstName a == firstName b
      && lastName a == lastName b
      && email a == email b
      && affiliation a == affiliation b
      && sources a == sources b
      && uRole a == uRole b
      && permissions a == permissions b
      && active a == active b
      && submissionProps a == submissionProps b
      && imageUrl a == imageUrl b
