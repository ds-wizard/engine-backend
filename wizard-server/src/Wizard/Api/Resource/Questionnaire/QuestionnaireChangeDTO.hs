module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeDTO
import Wizard.Model.Questionnaire.Questionnaire

data QuestionnaireChangeDTO = QuestionnaireChangeDTO
  { name :: String
  , description :: Maybe String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , projectTags :: [String]
  , permissions :: [QuestionnairePermChangeDTO]
  , documentTemplateId :: Maybe String
  , formatUuid :: Maybe U.UUID
  , isTemplate :: Bool
  }
  deriving (Show, Eq, Generic)
