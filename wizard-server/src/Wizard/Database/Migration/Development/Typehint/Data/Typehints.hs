module Wizard.Database.Migration.Development.Typehint.Data.Typehints where

import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

lifeScienceTypehint :: TypehintIDTO
lifeScienceTypehint =
  TypehintIDTO
    { intId = Just "op-p000001"
    , name = "Life Science Ontology"
    }

mathematicalTypehint :: TypehintIDTO
mathematicalTypehint =
  TypehintIDTO
    { intId = Just "op-p000008"
    , name = "Mathematical Ontology"
    }

legalTypehint :: TypehintIDTO
legalTypehint =
  TypehintIDTO
    { intId = Just "op-p000015"
    , name = "Legal Ontology"
    }

typehintRequest :: TypehintRequestDTO
typehintRequest =
  TypehintRequestDTO
    { packageId = Just germanyPackage.pId
    , events = []
    , questionUuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , q = "dog"
    }
