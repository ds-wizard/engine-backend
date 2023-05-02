module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireVersionChangeDTO = QuestionnaireVersionChangeDTO
  { name :: String
  , description :: Maybe String
  , eventUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
