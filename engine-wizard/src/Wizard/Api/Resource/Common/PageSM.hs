module Wizard.Api.Resource.Common.PageSM where

import Data.Swagger

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Database.Migration.Development.Common.Data.Pages
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Common.Page
import qualified Shared.Service.Package.PackageMapper as SP_Mapper
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
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Service.Package.PackageMapper as P_Mapper
import qualified Wizard.Service.User.UserMapper as U_Mapper

instance ToSchema (Page UserDTO) where
  declareNamedSchema = simpleToSchema' "_page" (Page "users" pageMetadata [U_Mapper.toDTO userAlbert])

instance ToSchema (Page PackageSimpleDTO) where
  declareNamedSchema =
    simpleToSchema' "_page" (Page "packages" pageMetadata [P_Mapper.toSimpleDTO (SP_Mapper.toPackage globalPackage)])

instance ToSchema (Page BranchDTO) where
  declareNamedSchema = simpleToSchema' "_page" (Page "branches" pageMetadata [amsterdamBranch])

instance ToSchema (Page QuestionnaireDTO) where
  declareNamedSchema = simpleToSchema' "_page" (Page "questionnaires" pageMetadata [questionnaire1Dto])

instance ToSchema (Page TemplateSimpleDTO) where
  declareNamedSchema = simpleToSchema' "_page" (Page "templates" pageMetadata [commonWizardTemplateSimpleDTO])

instance ToSchema (Page DocumentDTO) where
  declareNamedSchema = simpleToSchema' "_page" (Page "documents" pageMetadata [doc1Dto])
