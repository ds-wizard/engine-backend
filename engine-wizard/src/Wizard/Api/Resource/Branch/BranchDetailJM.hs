module Wizard.Api.Resource.Branch.BranchDetailJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchStateJM ()
import Wizard.Api.Resource.Package.PackageSimpleJM ()

instance FromJSON BranchDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BranchDetailDTO where
  toJSON = genericToJSON jsonOptions
