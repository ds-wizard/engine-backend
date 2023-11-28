module Wizard.Database.Migration.Development.Typehint.Data.Typehints where

import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

lifeScienceTypehint :: TypehintDTO
lifeScienceTypehint =
  TypehintDTO
    { intId = Just "op-p000001"
    , name = "Life Science Ontology"
    , url = Just "https://example.com/ontologies/${id}"
    }

mathematicalTypehint :: TypehintDTO
mathematicalTypehint =
  TypehintDTO
    { intId = Just "op-p000008"
    , name = "Mathematical Ontology"
    , url = Just "https://example.com/ontologies/${id}"
    }

legalTypehint :: TypehintDTO
legalTypehint =
  TypehintDTO
    { intId = Just "op-p000015"
    , name = "Legal Ontology"
    , url = Just "https://example.com/ontologies/${id}"
    }

typehintRequest :: TypehintRequestDTO
typehintRequest =
  TypehintRequestDTO
    { packageId = Just germanyPackage.pId
    , events = []
    , questionUuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , q = "dog"
    }
