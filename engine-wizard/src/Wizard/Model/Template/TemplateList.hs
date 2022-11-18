module Wizard.Model.Template.TemplateList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template
import Wizard.Model.Template.TemplateState

data TemplateList = TemplateList
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , recommendedPackageId :: Maybe String
  , formats :: [TemplateFormat]
  , state :: TemplateState
  , remoteVersion :: Maybe String
  , remoteOrganizationName :: Maybe String
  , remoteOrganizationLogo :: Maybe String
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord TemplateList where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.templateId b.templateId
      <> compare a.version b.version
