module Wizard.Database.Migration.Development.Typehint.Data.Typehints where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Api.Resource.Typehint.TypehintRequestDTO

lifeScienceTypehint :: TypehintDTO
lifeScienceTypehint =
  TypehintDTO
    { _typehintDTOIntId = "op-p000001"
    , _typehintDTOName = "Life Science Ontology"
    , _typehintDTOUrl = "https://example.com/ontologies/${id}"
    }

mathematicalTypehint :: TypehintDTO
mathematicalTypehint =
  TypehintDTO
    { _typehintDTOIntId = "op-p000008"
    , _typehintDTOName = "Mathematical Ontology"
    , _typehintDTOUrl = "https://example.com/ontologies/${id}"
    }

legalTypehint :: TypehintDTO
legalTypehint =
  TypehintDTO
    { _typehintDTOIntId = "op-p000015"
    , _typehintDTOName = "Legal Ontology"
    , _typehintDTOUrl = "https://example.com/ontologies/${id}"
    }

typehintRequest :: TypehintRequestDTO
typehintRequest =
  TypehintRequestDTO
    { _typehintRequestDTOPackageId = Just $ germanyPackage ^. pId
    , _typehintRequestDTOEvents = []
    , _typehintRequestDTOQuestionUuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
    , _typehintRequestDTOQ = "dog"
    }
