module Wizard.Api.Resource.Project.Detail.ProjectDetailReportDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.Project.Project
import Wizard.Model.Report.Report

data ProjectDetailReportDTO = ProjectDetailReportDTO
  { uuid :: U.UUID
  , name :: String
  , sharing :: ProjectSharing
  , visibility :: ProjectVisibility
  , knowledgeModelPackageId :: String
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , fileCount :: Int
  , totalReport :: TotalReport
  , chapterReports :: [ChapterReport]
  , chapters :: [Chapter]
  , metrics :: [Metric]
  }
  deriving (Show, Eq, Generic)
