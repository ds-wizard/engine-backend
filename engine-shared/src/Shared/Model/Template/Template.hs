module Shared.Model.Template.Template where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Package.PackagePattern

data Template =
  Template
    { _templateTId :: String
    , _templateName :: String
    , _templateOrganizationId :: String
    , _templateTemplateId :: String
    , _templateVersion :: String
    , _templateMetamodelVersion :: Int
    , _templateDescription :: String
    , _templateReadme :: String
    , _templateLicense :: String
    , _templateAllowedPackages :: [PackagePattern]
    , _templateRecommendedPackageId :: Maybe String
    , _templateFormats :: [TemplateFormat]
    , _templateCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data TemplateFormat =
  TemplateFormat
    { _templateFormatUuid :: U.UUID
    , _templateFormatName :: String
    , _templateFormatShortName :: String
    , _templateFormatIcon :: String
    , _templateFormatColor :: String
    , _templateFormatSteps :: [TemplateFormatStep]
    }
  deriving (Show, Eq, Generic)

data TemplateFormatStep =
  TemplateFormatStep
    { _templateFormatStepName :: String
    , _templateFormatStepOptions :: M.Map String String
    }
  deriving (Show, Eq, Generic)

data TemplateFile =
  TemplateFile
    { _templateFileTemplateId :: String
    , _templateFileUuid :: U.UUID
    , _templateFileFileName :: String
    , _templateFileContent :: String
    }
  deriving (Show, Eq, Generic)

data TemplateAsset =
  TemplateAsset
    { _templateAssetTemplateId :: String
    , _templateAssetUuid :: U.UUID
    , _templateAssetFileName :: String
    , _templateAssetContentType :: String
    }
  deriving (Show, Eq, Generic)

instance Ord Template where
  compare a b =
    compare (_templateOrganizationId a) (_templateOrganizationId b) <>
    compare (_templateTemplateId a) (_templateTemplateId b) <> compare (_templateVersion a) (_templateVersion b)
