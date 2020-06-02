module Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO where

import GHC.Generics

import Wizard.Api.Resource.Report.ReportDTO

data QuestionnaireReportDTO =
  QuestionnaireReportDTO
    { _questionnaireReportDTOIndications :: [IndicationDTO]
    }
  deriving (Show, Eq, Generic)
