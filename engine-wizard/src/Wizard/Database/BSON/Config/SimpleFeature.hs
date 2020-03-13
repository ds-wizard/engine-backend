module Wizard.Database.BSON.Config.SimpleFeature where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Wizard.Database.BSON.Common ()
import Wizard.Model.Config.SimpleFeature

instance ToBSON SimpleFeature where
  toBSON SimpleFeature {..} = ["enabled" BSON.=: _simpleFeatureEnabled]

instance FromBSON SimpleFeature where
  fromBSON doc = do
    _simpleFeatureEnabled <- BSON.lookup "enabled" doc
    return SimpleFeature {..}
