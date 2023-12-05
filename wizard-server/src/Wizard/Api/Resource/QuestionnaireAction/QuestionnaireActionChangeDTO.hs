module Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeDTO where

import GHC.Generics

data QuestionnaireActionChangeDTO = QuestionnaireActionChangeDTO
  { enabled :: Bool
  }
  deriving (Show, Eq, Generic)
