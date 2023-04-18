module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSuggestionDTO

data QuestionnaireVersionDTO = QuestionnaireVersionDTO
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , eventUuid :: U.UUID
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq QuestionnaireVersionDTO where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.eventUuid == b.eventUuid
      && a.createdBy == b.createdBy
