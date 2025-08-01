module Wizard.Database.Migration.Development.TypeHint.Data.TypeHints where

import Data.Aeson
import qualified Data.Map.Strict as M

import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Model.Branch.Branch
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

lifeScienceTypeHint :: TypeHintIDTO
lifeScienceTypeHint =
  TypeHintIDTO
    { valueForSelection = Just "op-p000001"
    , value = "Life Science Ontology"
    , raw = object []
    }

mathematicalTypeHint :: TypeHintIDTO
mathematicalTypeHint =
  TypeHintIDTO
    { valueForSelection = Just "op-p000008"
    , value = "Mathematical Ontology"
    , raw = object []
    }

legalTypeHint :: TypeHintIDTO
legalTypeHint =
  TypeHintIDTO
    { valueForSelection = Just "op-p000015"
    , value = "Legal Ontology"
    , raw = object []
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

typeHintLegacyRequest :: TypeHintRequestDTO
typeHintLegacyRequest =
  TypeHintRequestDTO
    { packageId = Just germanyPackage.pId
    , events = []
    , questionUuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , q = "dog"
    }

typeHintRequest :: TypeHintRequestDTO
typeHintRequest =
  TypeHintRequestDTO
    { packageId = Just germanyPackage.pId
    , events = []
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
