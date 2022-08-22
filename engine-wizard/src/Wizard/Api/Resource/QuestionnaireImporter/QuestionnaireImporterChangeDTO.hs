module Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterChangeDTO where

import GHC.Generics

data QuestionnaireImporterChangeDTO =
  QuestionnaireImporterChangeDTO
    { _questionnaireImporterChangeDTOEnabled :: Bool
    }
  deriving (Show, Eq, Generic)
