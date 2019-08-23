module Database.BSON.Level.Level where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import LensesConfig
import Model.Level.Level

instance ToBSON Level where
  toBSON l =
    [ "level" BSON.=: (l ^. level)
    , "title" BSON.=: (l ^. title)
    , "description" BSON.=: (l ^. description)
    , "createdAt" BSON.=: (l ^. createdAt)
    , "updatedAt" BSON.=: (l ^. updatedAt)
    ]

instance FromBSON Level where
  fromBSON doc = do
    lLevel <- BSON.lookup "level" doc
    lTitle <- BSON.lookup "title" doc
    lDescription <- BSON.lookup "description" doc
    lCreatedAt <- BSON.lookup "createdAt" doc
    lUpdatedAt <- BSON.lookup "updatedAt" doc
    return
      Level
      { _levelLevel = lLevel
      , _levelTitle = lTitle
      , _levelDescription = lDescription
      , _levelCreatedAt = lCreatedAt
      , _levelUpdatedAt = lUpdatedAt
      }
