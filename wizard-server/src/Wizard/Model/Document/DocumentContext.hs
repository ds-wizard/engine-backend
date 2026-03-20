module Wizard.Model.Document.DocumentContext where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.SemVer2Tuple
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Project.File.ProjectFileSimple
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Project.Version.ProjectVersionList
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Report.Report
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailDTO

data DocumentContext = DocumentContext
  { config :: DocumentContextConfig
  , document :: DocumentContextDocument
  , project :: DocumentContextProject
  , knowledgeModel :: KnowledgeModel
  , report :: Report
  , knowledgeModelPackage :: DocumentContextPackage
  , organization :: TenantConfigOrganization
  , metamodelVersion :: SemVer2Tuple
  , users :: [DocumentContextUserPerm]
  , groups :: [DocumentContextUserGroupPerm]
  }
  deriving (Show, Eq, Generic)

data DocumentContextConfig = DocumentContextConfig
  { clientUrl :: String
  , appTitle :: Maybe String
  , appTitleShort :: Maybe String
  , illustrationsColor :: Maybe String
  , primaryColor :: Maybe String
  , logoUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)

data DocumentContextPackage = DocumentContextPackage
  { uuid :: U.UUID
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

data DocumentContextUser = DocumentContextUser
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , affiliation :: Maybe String
  , active :: Bool
  , imageUrl :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextProject = DocumentContextProject
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , replies :: M.Map String Reply
  , phaseUuid :: Maybe U.UUID
  , labels :: M.Map String [U.UUID]
  , versionUuid :: Maybe U.UUID
  , versions :: [ProjectVersionList]
  , projectTags :: [String]
  , files :: [ProjectFileSimple]
  , createdBy :: Maybe DocumentContextUser
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextDocument = DocumentContextDocument
  { uuid :: U.UUID
  , name :: String
  , documentTemplateId :: String
  , formatUuid :: U.UUID
  , createdBy :: Maybe DocumentContextUser
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DocumentContextUserPerm = DocumentContextUserPerm
  { user :: DocumentContextUser
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)

data DocumentContextUserGroupPerm = DocumentContextUserGroupPerm
  { group :: UserGroupDetailDTO
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)
