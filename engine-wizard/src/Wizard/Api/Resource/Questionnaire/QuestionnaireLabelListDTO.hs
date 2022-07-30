module Wizard.Api.Resource.Questionnaire.QuestionnaireLabelListDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireLabelListDTO =
  QuestionnaireLabelListDTO
    { _questionnaireLabelListDTOLabelThreadsMap :: M.Map String [U.UUID]
    }
  deriving (Show, Eq, Generic)
