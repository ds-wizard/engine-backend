module Wizard.Database.BSON.Level.Level where

import Data.Bson.Generic

import Wizard.Model.Level.Level

instance ToBSON Level

instance FromBSON Level
