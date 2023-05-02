module Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO where

import Data.Time
import GHC.Generics

data QuestionnaireImporterDTO = QuestionnaireImporterDTO
  { qiId :: String
  , name :: String
  , description :: String
  , url :: String
  , enabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
