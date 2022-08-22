module Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO where

import Data.Time
import GHC.Generics

data QuestionnaireImporterDTO =
  QuestionnaireImporterDTO
    { _questionnaireImporterDTOQiId :: String
    , _questionnaireImporterDTOName :: String
    , _questionnaireImporterDTODescription :: String
    , _questionnaireImporterDTOUrl :: String
    , _questionnaireImporterDTOEnabled :: Bool
    , _questionnaireImporterDTOCreatedAt :: UTCTime
    , _questionnaireImporterDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
