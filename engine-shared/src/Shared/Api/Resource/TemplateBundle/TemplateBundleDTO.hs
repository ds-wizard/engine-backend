module Shared.Api.Resource.TemplateBundle.TemplateBundleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Template.TemplateDTO
import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template

data TemplateBundleDTO = TemplateBundleDTO
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
  , files :: [TemplateFileDTO]
  , assets :: [TemplateAssetDTO]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
