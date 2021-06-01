module Shared.Model.Package.PackagePattern where

import GHC.Generics

data PackagePattern =
  PackagePattern
    { _packagePatternOrgId :: Maybe String
    , _packagePatternKmId :: Maybe String
    , _packagePatternMinVersion :: Maybe String
    , _packagePatternMaxVersion :: Maybe String
    }
  deriving (Show, Eq, Generic)
