module Database.BSON.KnowledgeModel.Path where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import LensesConfig
import Model.KnowledgeModel.Path

instance ToBSON PathItem where
  toBSON value = ["type" BSON.=: value ^. pType, "uuid" BSON.=: (value ^. uuid)]

instance FromBSON PathItem where
  fromBSON doc = do
    epType <- BSON.lookup "type" doc
    epUuid <- BSON.lookup "uuid" doc
    return PathItem {_pathItemPType = epType, _pathItemUuid = epUuid}
