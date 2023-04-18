module Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeDTO where

import GHC.Generics

data QuestionnaireImporterChangeDTO = QuestionnaireImporterChangeDTO
  { enabled :: Bool
  }
  deriving (Show, Eq, Generic)
