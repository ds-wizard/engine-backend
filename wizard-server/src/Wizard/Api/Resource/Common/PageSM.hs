module Wizard.Api.Resource.Common.PageSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.PageJM ()
import Shared.Common.Api.Resource.Common.PageMetadataSM ()
import Shared.Common.Database.Migration.Development.Common.Data.Pages
import Shared.Common.Model.Common.Page
import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionSM ()
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Locale.Api.Resource.Locale.LocaleSuggestionSM ()
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.LocaleSuggestion
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentSM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListSM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListSM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM ()
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Wizard.Api.Resource.Project.Action.ProjectActionDTO
import Wizard.Api.Resource.Project.Action.ProjectActionSM ()
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadAssignedSM ()
import Wizard.Api.Resource.Project.Event.ProjectEventListSM ()
import Wizard.Api.Resource.Project.File.ProjectFileListSM ()
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterSM ()
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectSM ()
import Wizard.Api.Resource.Project.ProjectSuggestionSM ()
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantSM ()
import Wizard.Api.Resource.User.Group.UserGroupSuggestionSM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.Locale.Data.Locales
import Wizard.Database.Migration.Development.Project.Data.ProjectActions
import Wizard.Database.Migration.Development.Project.Data.ProjectComments
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.ProjectFiles
import Wizard.Database.Migration.Development.Project.Data.ProjectImporters
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Model.Project.Comment.ProjectCommentThreadAssigned
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.File.ProjectFileList
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectSuggestion
import Wizard.Model.User.UserGroupSuggestion
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import qualified Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as P_Mapper
import Wizard.Service.Project.Event.ProjectEventMapper
import qualified Wizard.Service.Project.ProjectMapper as PRJ_Mapper
import qualified Wizard.Service.Tenant.TenantMapper as TNT_Mapper
import qualified Wizard.Service.User.Group.UserGroupMapper as UG_Mapper
import qualified Wizard.Service.User.UserMapper as U_Mapper
import WizardLib.Public.Api.Resource.PersistentCommand.PersistentCommandListSM ()
import WizardLib.Public.Api.Resource.User.UserSuggestionSM ()
import WizardLib.Public.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.PersistentCommand.PersistentCommandList
import WizardLib.Public.Model.User.UserSuggestion

instance ToSchema (Page String) where
  declareNamedSchema = toSwaggerWithDtoName "Page String" (Page "projectTags" pageMetadata ["value1"])

instance ToSchema (Page UserDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page UserDTO" (Page "users" pageMetadata [U_Mapper.toDTO userAlbert])

instance ToSchema (Page UserSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page UserSuggestion"
      (Page "users" pageMetadata [U_Mapper.toSuggestion . U_Mapper.toSimple $ userAlbert])

instance ToSchema (Page UserGroupSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page UserGroupSuggestion" (Page "userGroups" pageMetadata [UG_Mapper.toSuggestion bioGroup])

instance ToSchema (Page LocaleDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page Locale" (Page "locales" pageMetadata [localeNlDto])

instance ToSchema (Page LocaleSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page LocaleSuggestion" (Page "locales" pageMetadata [localeNlSuggestion])

instance ToSchema (Page KnowledgeModelPackageSimpleDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page KnowledgeModelPackageSimpleDTO"
      (Page "knowledgeModelPackages" pageMetadata [P_Mapper.toSimpleDTO globalKmPackage])

instance ToSchema (Page KnowledgeModelPackageSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page KnowledgeModelPackageSuggestion"
      (Page "knowledgeModelPackages" pageMetadata [P_Mapper.toSuggestion globalKmPackage])

instance ToSchema (Page KnowledgeModelEditorList) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page KnowledgeModelEditorList" (Page "knowledgeModelEditors" pageMetadata [amsterdamKnowledgeModelEditorList])

instance ToSchema (Page KnowledgeModelEditorSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page KnowledgeModelEditorSuggestion" (Page "knowledgeModelEditors" pageMetadata [amsterdamKnowledgeModelEditorSuggestion])

instance ToSchema (Page ProjectDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page ProjectDTO" (Page "projects" pageMetadata [project1Dto])

instance ToSchema (Page ProjectSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page ProjectSuggestion" (Page "projects" pageMetadata [PRJ_Mapper.toSuggestion project1])

instance ToSchema (Page ProjectCommentThreadAssigned) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page ProjectCommentThreadAssigned" (Page "commentThreads" pageMetadata [cmtAssigned])

instance ToSchema (Page ProjectActionDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page ProjectActionDTO"
      (Page "projectActions" pageMetadata [projectActionFtp3Dto])

instance ToSchema (Page ProjectEventList) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page ProjectEventList"
      (Page "projectEvents" pageMetadata [SetReplyEventList' (toSetReplyEventList (sre_rQ1 project1.uuid) (Just userAlbert))])

instance ToSchema (Page ProjectFileList) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page ProjectFileList"
      (Page "projectFiles" pageMetadata [projectFileList])

instance ToSchema (Page ProjectImporterDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page ProjectImporterDTO"
      (Page "projectImporters" pageMetadata [projectImporterBio3Dto])

instance ToSchema (Page DocumentTemplateSimpleDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page DocumentTemplateSimpleDTO" (Page "documentTemplates" pageMetadata [wizardDocumentTemplateSimpleDTO])

instance ToSchema (Page DocumentTemplateSuggestionDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page DocumentTemplateSuggestionDTO"
      (Page "documentTemplates" pageMetadata [toSuggestionDTO wizardDocumentTemplate wizardDocumentTemplateFormats])

instance ToSchema (Page DocumentDTO) where
  declareNamedSchema = toSwaggerWithDtoName "Page DocumentDTO" (Page "documents" pageMetadata [doc1Dto])

instance ToSchema (Page PersistentCommandList) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page PersistentCommandList"
      ( Page
          "persistentCommands"
          pageMetadata
          [command1List]
      )

instance ToSchema (Page TenantDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page TenantDTO" (Page "tenants" pageMetadata [TNT_Mapper.toDTO defaultTenant Nothing Nothing])

instance ToSchema (Page DocumentTemplateDraftList) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page DocumentTemplateDraftList" (Page "documentTemplateDrafts" pageMetadata [toDraftList wizardDocumentTemplate])
