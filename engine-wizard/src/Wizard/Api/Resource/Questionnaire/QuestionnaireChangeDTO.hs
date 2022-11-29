module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl

data QuestionnaireChangeDTO = QuestionnaireChangeDTO
  { name :: String
  , description :: Maybe String
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , projectTags :: [String]
  , permissions :: [QuestionnairePermRecord]
  , templateId :: Maybe String
  , formatUuid :: Maybe U.UUID
  , isTemplate :: Bool
  }
  deriving (Show, Eq, Generic)
