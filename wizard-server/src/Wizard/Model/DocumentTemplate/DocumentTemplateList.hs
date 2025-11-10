module Wizard.Model.DocumentTemplate.DocumentTemplateList where

import Data.Time
import GHC.Generics

import Shared.Common.Model.Common.SemVer2Tuple
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern

data DocumentTemplateList = DocumentTemplateList
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: SemVer2Tuple
  , description :: String
  , allowedPackages :: [KnowledgeModelPackagePattern]
  , nonEditable :: Bool
  , remoteVersion :: Maybe String
  , remoteOrganizationName :: Maybe String
  , remoteOrganizationLogo :: Maybe String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplateList where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.templateId b.templateId
      <> compare a.version b.version
