module Wizard.Api.Resource.Package.PackageChangeDTO where

import GHC.Generics

import Shared.Model.Package.Package

data PackageChangeDTO = PackageChangeDTO
  { phase :: PackagePhase
  }
  deriving (Show, Eq, Generic)
