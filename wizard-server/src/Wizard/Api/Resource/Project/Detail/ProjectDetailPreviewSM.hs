module Wizard.Api.Resource.Project.Detail.ProjectDetailPreviewSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import qualified Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as DocumentTemplateMapper
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermSM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailPreviewJM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.Detail.ProjectDetailPreview
import Wizard.Model.Project.Project

instance ToSchema ProjectDetailPreview where
  declareNamedSchema =
    toSwagger $
      ProjectDetailPreview
        { uuid = project1.uuid
        , name = project1.name
        , visibility = project1.visibility
        , sharing = project1.sharing
        , knowledgeModelPackageId = project1.knowledgeModelPackageId
        , isTemplate = project1.isTemplate
        , documentTemplateId = Just wizardDocumentTemplate.tId
        , migrationUuid = Nothing
        , permissions = [project1AlbertEditProjectPermDto]
        , format = Just . DocumentTemplateMapper.toFormatSimple $ formatJson
        , fileCount = 0
        }
