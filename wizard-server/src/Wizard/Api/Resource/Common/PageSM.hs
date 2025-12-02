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
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventListSM ()
import Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileListSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadAssignedSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionSM ()
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionSM ()
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterSM ()
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantSM ()
import Wizard.Api.Resource.User.Group.UserGroupSuggestionSM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.Locale.Data.Locales
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireFiles
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.QuestionnaireAction.Data.QuestionnaireActions
import Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireCommentThreadAssigned
import Wizard.Model.Questionnaire.QuestionnaireEventList
import Wizard.Model.Questionnaire.QuestionnaireFileList
import Wizard.Model.Questionnaire.QuestionnaireSuggestion
import Wizard.Model.User.UserGroupSuggestion
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import qualified Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as P_Mapper
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QTN_Mapper
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

instance ToSchema (Page QuestionnaireDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page QuestionnaireDTO" (Page "questionnaires" pageMetadata [questionnaire1Dto])

instance ToSchema (Page QuestionnaireSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page QuestionnaireSuggestion" (Page "questionnaires" pageMetadata [QTN_Mapper.toSuggestion questionnaire1])

instance ToSchema (Page QuestionnaireCommentThreadAssigned) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page QuestionnaireCommentThreadAssigned" (Page "commentThreads" pageMetadata [cmtAssigned])

instance ToSchema (Page QuestionnaireActionDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page QuestionnaireActionDTO"
      (Page "questionnaireActions" pageMetadata [questionnaireActionFtp3Dto])

instance ToSchema (Page QuestionnaireEventList) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page QuestionnaireEventList"
      (Page "questionnaireEvents" pageMetadata [SetReplyEventList' (toSetReplyEventList (sre_rQ1 questionnaire1.uuid) (Just userAlbert))])

instance ToSchema (Page QuestionnaireFileList) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page QuestionnaireFileList"
      (Page "questionnaireFiles" pageMetadata [questionnaireFileList])

instance ToSchema (Page QuestionnaireImporterDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page QuestionnaireImporterDTO"
      (Page "questionnaireImporters" pageMetadata [questionnaireImporterBio3Dto])

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
