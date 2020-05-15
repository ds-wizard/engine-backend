module Wizard.Api.Resource.Template.TemplateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Model.Template.Template

data TemplateDTO =
  TemplateDTO
    { _templateDTOUuid :: U.UUID
    , _templateDTOName :: String
    , _templateDTODescription :: String
    , _templateDTOAllowedPackages :: [PackageSimpleDTO]
    , _templateDTORecommendedPackageId :: Maybe String
    , _templateDTOFormats :: [TemplateFormat]
    }
  deriving (Show, Eq, Generic)
