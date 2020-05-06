module Registry.Database.BSON.Organization.Organization where

import Data.Bson.Generic

import Registry.Database.BSON.Organization.OrganizationRole ()
import Registry.Model.Organization.Organization

instance ToBSON Organization

instance FromBSON Organization
