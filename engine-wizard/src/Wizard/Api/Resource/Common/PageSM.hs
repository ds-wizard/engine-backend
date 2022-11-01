module Wizard.Api.Resource.Common.PageSM where

import Data.Swagger

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Api.Resource.Template.TemplateSuggestionSM ()
import Shared.Database.Migration.Development.Common.Data.Pages
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Common.Page
import qualified Shared.Service.Package.PackageMapper as SP_Mapper
import Shared.Service.Template.TemplateMapper
import Shared.Util.Swagger
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppSM ()
import Wizard.Api.Resource.Branch.BranchListSM ()
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentSM ()
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Package.PackageSuggestionSM ()
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDTO
import Wizard.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterSM ()
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Api.Resource.Template.TemplateSimpleSM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Branch.BranchList
import Wizard.Model.Package.PackageSuggestion
import qualified Wizard.Service.App.AppMapper as A_Mapper
import qualified Wizard.Service.Package.PackageMapper as P_Mapper
import qualified Wizard.Service.PersistentCommand.PersistentCommandMapper as PC_Mapper
import qualified Wizard.Service.User.UserMapper as U_Mapper

instance ToSchema (Page String) where
  declareNamedSchema = simpleToSchema''''' "_page" "Page String" (Page "projectTags" pageMetadata ["value1"])

instance ToSchema (Page UserDTO) where
  declareNamedSchema =
    simpleToSchema''''' "_page" "Page UserDTO" (Page "users" pageMetadata [U_Mapper.toDTO userAlbert])

instance ToSchema (Page UserSuggestionDTO) where
  declareNamedSchema =
    simpleToSchema'''''
      "_page"
      "Page UserSuggestionDTO"
      (Page "users" pageMetadata [U_Mapper.toSuggestionDTO . U_Mapper.toSuggestion $ userAlbert])

instance ToSchema (Page PackageSimpleDTO) where
  declareNamedSchema =
    simpleToSchema'''''
      "_page"
      "Page PackageSimpleDTO"
      (Page "packages" pageMetadata [P_Mapper.toSimpleDTO (SP_Mapper.toPackage globalPackage)])

instance ToSchema (Page PackageSuggestion) where
  declareNamedSchema =
    simpleToSchema'''''
      "_page"
      "Page PackageSuggestion"
      (Page "packages" pageMetadata [P_Mapper.toSuggestion (SP_Mapper.toPackage globalPackage, [])])

instance ToSchema (Page BranchList) where
  declareNamedSchema =
    simpleToSchema''''' "_page" "Page BranchList" (Page "branches" pageMetadata [amsterdamBranchList])

instance ToSchema (Page QuestionnaireDTO) where
  declareNamedSchema =
    simpleToSchema''''' "_page" "Page QuestionnaireDTO" (Page "questionnaires" pageMetadata [questionnaire1Dto])

instance ToSchema (Page QuestionnaireImporterDTO) where
  declareNamedSchema =
    simpleToSchema'''''
      "_page"
      "Page QuestionnaireImporterDTO"
      (Page "questionnaireImporters" pageMetadata [questionnaireImporterBio3Dto])

instance ToSchema (Page TemplateSimpleDTO) where
  declareNamedSchema =
    simpleToSchema''''' "_page" "Page TemplateSimpleDTO" (Page "templates" pageMetadata [commonWizardTemplateSimpleDTO])

instance ToSchema (Page TemplateSuggestionDTO) where
  declareNamedSchema =
    simpleToSchema'''''
      "_page"
      "Page TemplateSuggestionDTO"
      (Page "templates" pageMetadata [toSuggestionDTO commonWizardTemplate])

instance ToSchema (Page DocumentDTO) where
  declareNamedSchema = simpleToSchema''''' "_page" "Page DocumentDTO" (Page "documents" pageMetadata [doc1Dto])

instance ToSchema (Page PersistentCommandDTO) where
  declareNamedSchema =
    simpleToSchema'''''
      "_page"
      "Page PersistentCommandDTO"
      (Page
         "persistentCommands"
         pageMetadata
         [PC_Mapper.toDTO command1 (Just userAlbert) (A_Mapper.toDTO defaultApp Nothing Nothing)])

instance ToSchema (Page AppDTO) where
  declareNamedSchema =
    simpleToSchema''''' "_page" "Page AppDTO" (Page "apps" pageMetadata [A_Mapper.toDTO defaultApp Nothing Nothing])
