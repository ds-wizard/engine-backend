module Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireSettingsChangeDTO = QuestionnaireSettingsChangeDTO
  { name :: String
  , description :: Maybe String
  , projectTags :: [String]
  , documentTemplateId :: Maybe String
  , formatUuid :: Maybe U.UUID
  , isTemplate :: Bool
  }
  deriving (Show, Eq, Generic)
