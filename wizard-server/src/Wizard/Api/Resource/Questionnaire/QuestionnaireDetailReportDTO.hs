module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailReportDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Questionnaire.QuestionnairePermDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Report.Report

data QuestionnaireDetailReportDTO = QuestionnaireDetailReportDTO
  { uuid :: U.UUID
  , name :: String
  , sharing :: QuestionnaireSharing
  , visibility :: QuestionnaireVisibility
  , knowledgeModelPackageId :: String
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
