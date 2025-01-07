module Wizard.Api.Resource.Branch.BranchDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchDetailJM ()
import Wizard.Api.Resource.Branch.BranchStateSM ()
import Wizard.Api.Resource.Package.PackageSimpleSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches
import WizardLib.KnowledgeModel.Api.Resource.Event.EventSM ()

instance ToSchema BranchDetailDTO where
  declareNamedSchema = toSwagger amsterdamBranchDetail
