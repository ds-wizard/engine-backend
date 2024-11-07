module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Report.Report
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data QuestionnaireDetailReportDTO = QuestionnaireDetailReportDTO
  { uuid :: U.UUID
  , name :: String
  , sharing :: QuestionnaireSharing
  , visibility :: QuestionnaireVisibility
  , packageId :: String
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [QuestionnairePermDTO]
  , fileCount :: Int
  , totalReport :: TotalReport
  , chapterReports :: [ChapterReport]
  , chapters :: [Chapter]
  , metrics :: [Metric]
  }
  deriving (Show, Eq, Generic)
