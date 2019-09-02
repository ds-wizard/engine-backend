module Api.Resource.Questionnaire.QuestionnaireLabelDTO where

import qualified Data.UUID as U
import GHC.Generics

data LabelDTO = LabelDTO
  { _labelDTOPath :: String
  , _labelDTOValue :: [U.UUID]
  } deriving (Show, Eq, Generic)
