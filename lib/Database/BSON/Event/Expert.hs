module Database.BSON.Event.Expert where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Database.BSON.KnowledgeModel.Path ()
import LensesConfig
import Model.Event.Expert.ExpertEvent

-- -------------------------
-- ADD EXPERT EVENT---------
-- -------------------------
instance ToBSON AddExpertEvent where
  toBSON event =
    [ "eventType" BSON.=: "AddExpertEvent"
    , "uuid" BSON.=: (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "expertUuid" BSON.=: (event ^. expertUuid)
    , "name" BSON.=: (event ^. name)
    , "email" BSON.=: (event ^. email)
    ]

instance FromBSON AddExpertEvent where
  fromBSON doc = do
    expUuid <- BSON.lookup "uuid" doc
    expPath <- BSON.lookup "path" doc
    expExpertUuid <- BSON.lookup "expertUuid" doc
    expName <- BSON.lookup "name" doc
    expEmail <- BSON.lookup "email" doc
    return
      AddExpertEvent
      { _addExpertEventUuid = expUuid
      , _addExpertEventPath = expPath
      , _addExpertEventExpertUuid = expExpertUuid
      , _addExpertEventName = expName
      , _addExpertEventEmail = expEmail
      }

-- -------------------------
-- EDIT EXPERT EVENT--------
-- -------------------------
instance ToBSON EditExpertEvent where
  toBSON event =
    [ "eventType" BSON.=: "EditExpertEvent"
    , "uuid" BSON.=: (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "expertUuid" BSON.=: (event ^. expertUuid)
    , "name" BSON.=: (event ^. name)
    , "email" BSON.=: (event ^. email)
    ]

instance FromBSON EditExpertEvent where
  fromBSON doc = do
    expUuid <- BSON.lookup "uuid" doc
    expPath <- BSON.lookup "path" doc
    expExpertUuid <- BSON.lookup "expertUuid" doc
    expName <- BSON.lookup "name" doc
    expEmail <- BSON.lookup "email" doc
    return
      EditExpertEvent
      { _editExpertEventUuid = expUuid
      , _editExpertEventPath = expPath
      , _editExpertEventExpertUuid = expExpertUuid
      , _editExpertEventName = expName
      , _editExpertEventEmail = expEmail
      }

-- -------------------------
-- DELETE EXPERT EVENT------
-- -------------------------
instance ToBSON DeleteExpertEvent where
  toBSON event =
    [ "eventType" BSON.=: "DeleteExpertEvent"
    , "uuid" BSON.=: (event ^. uuid)
    , "path" BSON.=: (event ^. path)
    , "expertUuid" BSON.=: (event ^. expertUuid)
    ]

instance FromBSON DeleteExpertEvent where
  fromBSON doc = do
    expUuid <- BSON.lookup "uuid" doc
    expPath <- BSON.lookup "path" doc
    expExpertUuid <- BSON.lookup "expertUuid" doc
    return
      DeleteExpertEvent
      {_deleteExpertEventUuid = expUuid, _deleteExpertEventPath = expPath, _deleteExpertEventExpertUuid = expExpertUuid}
