module Wizard.Api.Resource.Common.PageSM where

import Data.Swagger

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionSM ()
import Shared.Database.Migration.Development.Common.Data.Pages
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Common.Page
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import qualified Shared.Service.Package.PackageMapper as SP_Mapper
import Shared.Util.Swagger
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppSM ()
import Wizard.Api.Resource.Branch.BranchListSM ()
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentSM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListSM ()
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleSM ()
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Package.PackageSuggestionSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionSM ()
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterSM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Wizard.Database.Migration.Development.Locale.Data.Locales
import Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Branch.BranchList
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Model.Package.PackageSuggestion
import Wizard.Model.Questionnaire.QuestionnaireSuggestion
import qualified Wizard.Service.App.AppMapper as A_Mapper
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import qualified Wizard.Service.Package.PackageMapper as P_Mapper
import qualified Wizard.Service.PersistentCommand.PersistentCommandMapper as PC_Mapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QTN_Mapper
import qualified Wizard.Service.User.UserMapper as U_Mapper

instance ToSchema (Page String) where
  declareNamedSchema = toSwaggerWithDtoName "Page String" (Page "projectTags" pageMetadata ["value1"])

instance ToSchema (Page UserDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page UserDTO" (Page "users" pageMetadata [U_Mapper.toDTO userAlbert])

instance ToSchema (Page UserSuggestionDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page UserSuggestionDTO"
      (Page "users" pageMetadata [U_Mapper.toSuggestionDTO . U_Mapper.toSuggestion $ userAlbert])

instance ToSchema (Page LocaleDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page Locale" (Page "locales" pageMetadata [localeNlDto])

instance ToSchema (Page PackageSimpleDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page PackageSimpleDTO"
      (Page "packages" pageMetadata [P_Mapper.toSimpleDTO (SP_Mapper.toPackage globalPackage)])

instance ToSchema (Page PackageSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page PackageSuggestion"
      (Page "packages" pageMetadata [P_Mapper.toSuggestion (SP_Mapper.toPackage globalPackage, [])])

instance ToSchema (Page BranchList) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page BranchList" (Page "branches" pageMetadata [amsterdamBranchList])

instance ToSchema (Page QuestionnaireDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page QuestionnaireDTO" (Page "questionnaires" pageMetadata [questionnaire1Dto])

instance ToSchema (Page QuestionnaireSuggestion) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page QuestionnaireSuggestion" (Page "questionnaires" pageMetadata [QTN_Mapper.toSuggestion questionnaire1])

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
      (Page "documentTemplates" pageMetadata [toSuggestionDTO wizardDocumentTemplate])

instance ToSchema (Page DocumentDTO) where
  declareNamedSchema = toSwaggerWithDtoName "Page DocumentDTO" (Page "documents" pageMetadata [doc1Dto])

instance ToSchema (Page PersistentCommandDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName
      "Page PersistentCommandDTO"
      ( Page
          "persistentCommands"
          pageMetadata
          [PC_Mapper.toDTO command1 (Just userAlbert) (A_Mapper.toDTO defaultApp Nothing Nothing)]
      )

instance ToSchema (Page AppDTO) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page AppDTO" (Page "apps" pageMetadata [A_Mapper.toDTO defaultApp Nothing Nothing])

instance ToSchema (Page DocumentTemplateDraftList) where
  declareNamedSchema =
    toSwaggerWithDtoName "Page DocumentTemplateDraftList" (Page "documentTemplateDrafts" pageMetadata [toDraftList wizardDocumentTemplate])
