module Wizard.Api.Resource.Project.Detail.ProjectDetailReportSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermSM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailReportJM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Api.Resource.Report.ReportSM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Model.Project.Project
import Wizard.Model.Report.Report

instance ToSchema ProjectDetailReportDTO where
  declareNamedSchema =
    toSwagger $
      ProjectDetailReportDTO
        { uuid = project1.uuid
        , name = project1.name
        , visibility = project1.visibility
        , sharing = project1.sharing
        , knowledgeModelPackageId = project1.knowledgeModelPackageId
        , isTemplate = project1.isTemplate
        , migrationUuid = Nothing
        , permissions = [project1AlbertEditProjectPermDto]
        , fileCount = 0
        , totalReport = report1.totalReport
        , chapterReports = report1.chapterReports
        , chapters = report1.chapters
        , metrics = report1.metrics
        }
