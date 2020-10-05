module Wizard.Database.BSON.ActionKey.ActionKeyType where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Wizard.Model.ActionKey.ActionKey

instance BSON.Val ActionKeyType where
  val = genericVal
  cast' = genericCast'
