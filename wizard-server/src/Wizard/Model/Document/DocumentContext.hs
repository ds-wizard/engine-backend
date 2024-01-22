module Wizard.Model.Document.DocumentContext where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Report.Report
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data DocumentContext = DocumentContext
  { uuid :: U.UUID
  , config :: DocumentContextConfig
  , questionnaireUuid :: String
  , questionnaireName :: String
  , questionnaireDescription :: Maybe String
  , questionnaireReplies :: M.Map String Reply
  , questionnaireVersion :: Maybe U.UUID
  , questionnaireVersions :: [QuestionnaireVersionDTO]
  , questionnaireProjectTags :: [String]
  , phaseUuid :: Maybe U.UUID
  , knowledgeModel :: KnowledgeModel
  , report :: Report
  , package :: DocumentContextPackage
  , organization :: TenantConfigOrganization
  , documentTemplateMetamodelVersion :: Int
  , createdBy :: Maybe UserDTO
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq DocumentContext where
  a == b =
    a.uuid == b.uuid
      && a.config == b.config
      && a.questionnaireUuid == b.questionnaireUuid
      && a.questionnaireName == b.questionnaireName
      && a.questionnaireDescription == b.questionnaireDescription
      && a.questionnaireReplies == b.questionnaireReplies
      && a.questionnaireVersion == b.questionnaireVersion
      && a.questionnaireVersions == b.questionnaireVersions
      && a.questionnaireProjectTags == b.questionnaireProjectTags
      && a.phaseUuid == b.phaseUuid
      && a.knowledgeModel == b.knowledgeModel
      && a.report == b.report
      && a.package == b.package
      && a.organization == b.organization
      && a.documentTemplateMetamodelVersion == b.documentTemplateMetamodelVersion
      && a.createdBy == b.createdBy

data DocumentContextConfig = DocumentContextConfig
  { clientUrl :: String
  }
  deriving (Show, Eq, Generic)

data DocumentContextPackage = DocumentContextPackage
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , versions :: [String]
  , remoteLatestVersion :: Maybe String
  , description :: String
  , organization :: Maybe RegistryOrganization
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
