module Wizard.Model.Questionnaire.QuestionnaireFile where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data QuestionnaireFile = QuestionnaireFile
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  , questionnaireUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
