module Wizard.Database.Migration.Development.Typehint.Data.Typehints where

import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.PackageWithEvents
import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Api.Resource.Typehint.TypehintRequestDTO

lifeScienceTypehint :: TypehintDTO
lifeScienceTypehint =
  TypehintDTO
    { intId = "op-p000001"
    , name = "Life Science Ontology"
    , url = "https://example.com/ontologies/${id}"
    }

mathematicalTypehint :: TypehintDTO
mathematicalTypehint =
  TypehintDTO
    { intId = "op-p000008"
    , name = "Mathematical Ontology"
    , url = "https://example.com/ontologies/${id}"
    }

legalTypehint :: TypehintDTO
legalTypehint =
  TypehintDTO
    { intId = "op-p000015"
    , name = "Legal Ontology"
    , url = "https://example.com/ontologies/${id}"
    }

typehintRequest :: TypehintRequestDTO
typehintRequest =
  TypehintRequestDTO
    { packageId = Just germanyPackage.pId
    , events = []
    , questionUuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , q = "dog"
    }
