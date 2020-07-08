module Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO where

import GHC.Generics

import Wizard.Model.Report.Report

data QuestionnaireReportDTO =
  QuestionnaireReportDTO
    { _questionnaireReportDTOIndications :: [Indication]
    }
  deriving (Show, Eq, Generic)
