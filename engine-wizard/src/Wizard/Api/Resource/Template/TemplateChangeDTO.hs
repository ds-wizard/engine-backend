module Wizard.Api.Resource.Template.TemplateChangeDTO where

import GHC.Generics

import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template

data TemplateChangeDTO = TemplateChangeDTO
  { name :: String
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
  }
  deriving (Show, Eq, Generic)
