module Wizard.Model.Questionnaire.QuestionnaireFileSimple where

import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data QuestionnaireFileSimple = QuestionnaireFileSimple
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , fileSize :: Int64
  }
  deriving (Show, Eq, Generic)
