module Wizard.Model.Template.TemplateList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template
import Wizard.Model.Template.TemplateState

data TemplateList =
  TemplateList
    { _templateListTId :: String
    , _templateListName :: String
    , _templateListOrganizationId :: String
    , _templateListTemplateId :: String
    , _templateListVersion :: String
    , _templateListMetamodelVersion :: Int
    , _templateListDescription :: String
    , _templateListReadme :: String
    , _templateListLicense :: String
    , _templateListAllowedPackages :: [PackagePattern]
    , _templateListRecommendedPackageId :: Maybe String
    , _templateListFormats :: [TemplateFormat]
    , _templateListState :: TemplateState
    , _templateListRemoteVersion :: Maybe String
    , _templateListRemoteOrganizationName :: Maybe String
    , _templateListRemoteOrganizationLogo :: Maybe String
    , _templateListAppUuid :: U.UUID
    , _templateListCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

instance Ord TemplateList where
  compare a b =
    compare (_templateListOrganizationId a) (_templateListOrganizationId b) <>
    compare (_templateListTemplateId a) (_templateListTemplateId b) <>
    compare (_templateListVersion a) (_templateListVersion b)
