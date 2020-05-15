module Wizard.Model.Template.Template where

import qualified Data.UUID as U
import GHC.Generics

data Template =
  Template
    { _templateUuid :: U.UUID
    , _templateName :: String
    , _templateDescription :: String
    , _templateAllowedPackages :: [TemplateAllowedPackage]
    , _templateRecommendedPackageId :: Maybe String
    , _templateFormats :: [TemplateFormat]
    }
  deriving (Show, Eq, Generic)

data TemplateAllowedPackage =
  TemplateAllowedPackage
    { _templateAllowedPackageOrgId :: Maybe String
    , _templateAllowedPackageKmId :: Maybe String
    , _templateAllowedPackageMinVersion :: Maybe String
    , _templateAllowedPackageMaxVersion :: Maybe String
    }
  deriving (Show, Eq, Generic)

data TemplateFormat =
  TemplateFormat
    { _templateFormatUuid :: U.UUID
    , _templateFormatName :: String
    , _templateFormatShortName :: String
    , _templateFormatIcon :: String
    , _templateFormatColor :: String
    }
  deriving (Show, Eq, Generic)
