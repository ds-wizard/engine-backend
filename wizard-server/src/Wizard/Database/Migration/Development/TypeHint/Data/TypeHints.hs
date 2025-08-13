module Wizard.Database.Migration.Development.TypeHint.Data.TypeHints where

import Data.Aeson
import qualified Data.Map.Strict as M

import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Questionnaire.Questionnaire
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

forestDatasetTypeHint :: TypeHintIDTO
forestDatasetTypeHint =
  TypeHintIDTO
    { valueForSelection = Just "r000001: Forest Dataset"
    , value = "Forest Dataset (biology)"
    , raw = object ["id" .= "r000001", "name" .= "Forest Dataset", "domain" .= "biology", "country" .= "cz"]
    }

genomicDatasetTypeHint :: TypeHintIDTO
genomicDatasetTypeHint =
  TypeHintIDTO
    { valueForSelection = Just "r000002: Genomic Dataset"
    , value = "Genomic Dataset (biology)"
    , raw = object ["id" .= "r000002", "name" .= "Genomic Dataset", "domain" .= "biology", "country" .= "cz"]
    }

animalsDatasetTypeHint :: TypeHintIDTO
animalsDatasetTypeHint =
  TypeHintIDTO
    { valueForSelection = Just "r000003: Animals Dataset"
    , value = "Animals Dataset (biology)"
    , raw = object ["id" .= "r000003", "name" .= "Animals Dataset", "domain" .= "biology", "country" .= "cz"]
    }

lifeScienceLegacyTypeHint :: TypeHintLegacyIDTO
lifeScienceLegacyTypeHint =
  TypeHintLegacyIDTO
    { intId = Just "op-p000001"
    , name = "Life Science Ontology"
    }

mathematicalLegacyTypeHint :: TypeHintLegacyIDTO
mathematicalLegacyTypeHint =
  TypeHintLegacyIDTO
    { intId = Just "op-p000008"
    , name = "Mathematical Ontology"
    }

legalLegacyTypeHint :: TypeHintLegacyIDTO
legalLegacyTypeHint =
  TypeHintLegacyIDTO
    { intId = Just "op-p000015"
    , name = "Legal Ontology"
    }

typeHintLegacyRequest :: TypeHintLegacyRequestDTO
typeHintLegacyRequest =
  TypeHintLegacyRequestDTO
    { packageId = Just germanyPackage.pId
    , events = []
    , questionUuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , q = "dog"
    }

branchIntegrationTypeHintRequest :: TypeHintRequestDTO
branchIntegrationTypeHintRequest = BranchIntegrationTypeHintRequest' branchIntegrationTypeHintRequest'

branchQuestionTypeHintRequest :: TypeHintRequestDTO
branchQuestionTypeHintRequest = BranchQuestionTypeHintRequest' branchQuestionTypeHintRequest'

questionnaireTypeHintRequest :: TypeHintRequestDTO
questionnaireTypeHintRequest = QuestionnaireTypeHintRequest' questionnaireTypeHintRequest'

branchIntegrationTypeHintRequest' :: BranchIntegrationTypeHintRequest
branchIntegrationTypeHintRequest' =
  BranchIntegrationTypeHintRequest
    { branchUuid = amsterdamBranch.uuid
    , integrationUuid = repositoryApi.uuid
    }

branchQuestionTypeHintRequest' :: BranchQuestionTypeHintRequest
branchQuestionTypeHintRequest' =
  BranchQuestionTypeHintRequest
    { branchUuid = amsterdamBranch.uuid
    , questionUuid = question15.uuid
    , q = "dog"
    }

questionnaireTypeHintRequest' :: QuestionnaireTypeHintRequest
questionnaireTypeHintRequest' =
  QuestionnaireTypeHintRequest
    { questionnaireUuid = questionnaire15.uuid
    , questionUuid = question15.uuid
    , q = "dog"
    }

typeHintTestRequest :: TypeHintTestRequestDTO
typeHintTestRequest =
  TypeHintTestRequestDTO
    { branchUuid = amsterdamBranch.uuid
    , integrationUuid = repositoryApi.uuid
    , variables = M.fromList [("domain", "biology"), ("country", "cz")]
    , q = "biology"
    }
