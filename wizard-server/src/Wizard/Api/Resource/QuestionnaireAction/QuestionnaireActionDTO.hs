module Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO where

import Data.Time
import GHC.Generics

data QuestionnaireActionDTO = QuestionnaireActionDTO
  { qaId :: String
  , name :: String
  , description :: String
  , url :: String
  , enabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
