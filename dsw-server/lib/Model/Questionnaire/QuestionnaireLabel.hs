module Model.Questionnaire.QuestionnaireLabel where

import qualified Data.UUID as U
import GHC.Generics

data Label = Label
  { _labelPath :: String
  , _labelValue :: [U.UUID]
  } deriving (Show, Eq, Generic)
