module Registry.Database.BSON.ActionKey.ActionKeyType where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Registry.Model.ActionKey.ActionKey

instance BSON.Val ActionKeyType where
  val = genericVal
  cast' = genericCast'
