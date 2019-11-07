module Database.BSON.Event.EventField where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Model.Event.EventField

instance BSON.Val a => ToBSON (EventField a) where
  toBSON (ChangedValue value) = ["changed" BSON.=: True, "value" BSON.=: value]
  toBSON NothingChanged = ["changed" BSON.=: BSON.Bool False]

instance BSON.Val a => FromBSON (EventField a) where
  fromBSON doc = do
    efChanged <- BSON.lookup "changed" doc
    if efChanged
      then do
        efValue <- BSON.lookup "value" doc
        return $ ChangedValue efValue
      else return NothingChanged
