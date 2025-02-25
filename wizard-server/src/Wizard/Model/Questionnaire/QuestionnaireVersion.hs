module Wizard.Model.Questionnaire.QuestionnaireVersion where

import Data.Aeson ()
import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Util.Hashable ()

data QuestionnaireVersion = QuestionnaireVersion
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , eventUuid :: U.UUID
  , questionnaireUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq QuestionnaireVersion where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.eventUuid == b.eventUuid
      && a.createdBy == b.createdBy

instance Hashable QuestionnaireVersion
