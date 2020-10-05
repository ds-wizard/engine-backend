module Wizard.Database.BSON.Config.SimpleFeature where

import Data.Bson.Generic

import Shared.Database.BSON.Common ()
import Wizard.Model.Config.SimpleFeature

instance ToBSON SimpleFeature

instance FromBSON SimpleFeature
