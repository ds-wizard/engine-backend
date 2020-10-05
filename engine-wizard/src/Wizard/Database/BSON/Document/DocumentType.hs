module Wizard.Database.BSON.Document.DocumentType where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Wizard.Model.Document.Document

instance BSON.Val DocumentState where
  val = genericVal
  cast' = genericCast'

instance BSON.Val DocumentDurability where
  val = genericVal
  cast' = genericCast'
