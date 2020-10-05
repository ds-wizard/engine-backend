module Shared.Database.BSON.Package.PackageWithEvents where

import Data.Bson.Generic

import Shared.Database.BSON.Event.Common ()
import Shared.Model.Package.PackageWithEvents

instance ToBSON PackageWithEvents

instance FromBSON PackageWithEvents
