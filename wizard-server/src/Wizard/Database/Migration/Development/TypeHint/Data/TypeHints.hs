module Wizard.Database.Migration.Development.TypeHint.Data.TypeHints where

import Data.Aeson
import qualified Data.Map.Strict as M

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Integration.Resource.TypeHint.TypeHintIDTO
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.Project.Project

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
    { knowledgeModelPackageId = Just germanyKmPackage.pId
    , events = []
    , questionUuid = q4_it1_q6_aYes_followUpQuestion5.uuid
    , q = "dog"
    }

kmEditorIntegrationTypeHintRequest :: TypeHintRequestDTO
kmEditorIntegrationTypeHintRequest = KnowledgeModelEditorIntegrationTypeHintRequest' kmEditorIntegrationTypeHintRequest'

kmEditorQuestionTypeHintRequest :: TypeHintRequestDTO
kmEditorQuestionTypeHintRequest = KnowledgeModelEditorQuestionTypeHintRequest' kmEditorQuestionTypeHintRequest'

projectTypeHintRequest :: TypeHintRequestDTO
projectTypeHintRequest = ProjectTypeHintRequest' projectTypeHintRequest'

kmEditorIntegrationTypeHintRequest' :: KnowledgeModelEditorIntegrationTypeHintRequest
kmEditorIntegrationTypeHintRequest' =
  KnowledgeModelEditorIntegrationTypeHintRequest
    { knowledgeModelEditorUuid = amsterdamKnowledgeModelEditor.uuid
    , integrationUuid = repositoryApi.uuid
    }

kmEditorQuestionTypeHintRequest' :: KnowledgeModelEditorQuestionTypeHintRequest
kmEditorQuestionTypeHintRequest' =
  KnowledgeModelEditorQuestionTypeHintRequest
    { knowledgeModelEditorUuid = amsterdamKnowledgeModelEditor.uuid
    , questionUuid = question15.uuid
    , q = "dog"
    }

projectTypeHintRequest' :: ProjectTypeHintRequest
projectTypeHintRequest' =
  ProjectTypeHintRequest
    { projectUuid = project15.uuid
    , questionUuid = question15.uuid
    , q = "dog"
    }

typeHintTestRequest :: TypeHintTestRequestDTO
typeHintTestRequest =
  TypeHintTestRequestDTO
    { knowledgeModelEditorUuid = amsterdamKnowledgeModelEditor.uuid
    , integrationUuid = repositoryApi.uuid
    , variables = M.fromList [("domain", "biology"), ("country", "cz")]
    , q = "biology"
    }
