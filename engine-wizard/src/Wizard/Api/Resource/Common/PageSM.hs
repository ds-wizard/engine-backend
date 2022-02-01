module Wizard.Api.Resource.Common.PageSM where

import Data.Swagger

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Api.Resource.Package.PackageSuggestionDTO
import Shared.Api.Resource.Package.PackageSuggestionSM ()
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Api.Resource.Template.TemplateSuggestionSM ()
import Shared.Database.Migration.Development.Common.Data.Pages
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import qualified Shared.Service.Package.PackageMapper as SP_Mapper
import Shared.Service.Template.TemplateMapper
import Shared.Util.Swagger
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchSM ()
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentSM ()
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Api.Resource.Template.TemplateSimpleSM ()
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Api.Resource.User.UserSuggestionSM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Service.Package.PackageMapper as P_Mapper
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
      (Page
         "packages"
         pageMetadata
         [SP_Mapper.toSuggestionDTO (SP_Mapper.toPackage globalPackage) (Page "packages" (PageMetadata 20 0 0 0) [])])

instance ToSchema (Page PackageSuggestionDTO) where
  declareNamedSchema =
    simpleToSchema'''''
      "_page"
      "Page PackageSuggestionDTO"
      (Page "packages" pageMetadata [P_Mapper.toSimpleDTO (SP_Mapper.toPackage globalPackage)])

instance ToSchema (Page BranchDTO) where
  declareNamedSchema = simpleToSchema''''' "_page" "Page BranchDTO" (Page "branches" pageMetadata [amsterdamBranchDto])

instance ToSchema (Page QuestionnaireDTO) where
  declareNamedSchema =
    simpleToSchema''''' "_page" "Page QuestionnaireDTO" (Page "questionnaires" pageMetadata [questionnaire1Dto])

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
