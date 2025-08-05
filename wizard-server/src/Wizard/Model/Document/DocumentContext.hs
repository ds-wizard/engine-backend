module Wizard.Model.Document.DocumentContext where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.SemVer2Tuple
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Questionnaire.QuestionnaireFileSimple
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Report.Report
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailDTO

data DocumentContext = DocumentContext
  { config :: DocumentContextConfig
  , document :: DocumentContextDocument
  , questionnaire :: DocumentContextQuestionnaire
  , knowledgeModel :: KnowledgeModel
  , report :: Report
  , package :: DocumentContextPackage
  , organization :: TenantConfigOrganization
  , metamodelVersion :: SemVer2Tuple
  , users :: [DocumentContextUserPerm]
  , groups :: [DocumentContextUserGroupPerm]
  }
  deriving (Show, Eq, Generic)

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

data DocumentContextQuestionnaire = DocumentContextQuestionnaire
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , replies :: M.Map String Reply
  , phaseUuid :: Maybe U.UUID
  , labels :: M.Map String [U.UUID]
  , versionUuid :: Maybe U.UUID
  , versions :: [QuestionnaireVersionList]
  , projectTags :: [String]
  , files :: [QuestionnaireFileSimple]
  , createdBy :: Maybe UserDTO
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextDocument = DocumentContextDocument
  { uuid :: U.UUID
  , name :: String
  , documentTemplateId :: String
  , formatUuid :: U.UUID
  , createdBy :: Maybe UserDTO
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextUserPerm = DocumentContextUserPerm
  { user :: UserDTO
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)

data DocumentContextUserGroupPerm = DocumentContextUserGroupPerm
  { group :: UserGroupDetailDTO
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)
