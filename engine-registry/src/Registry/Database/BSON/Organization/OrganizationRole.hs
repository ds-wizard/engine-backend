module Registry.Database.BSON.Organization.OrganizationRole where

import qualified Data.Bson as BSON

import Data.Bson.Generic
import Registry.Model.Organization.Organization

instance BSON.Val OrganizationRole where
  val = genericVal
  cast' = genericCast'
