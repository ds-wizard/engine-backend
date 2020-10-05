module Shared.Database.BSON.Package.PackageGroup where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Shared.Database.BSON.Package.Package ()
import Shared.Model.Package.PackageGroup

instance ToBSON PackageGroup

instance FromBSON PackageGroup
