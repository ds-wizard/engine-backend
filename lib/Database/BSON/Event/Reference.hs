module Database.BSON.Event.Reference where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Database.BSON.Event.EventField ()
import Database.BSON.KnowledgeModel.Path ()
import LensesConfig
import Model.Event.Reference.ReferenceEvent

-- -------------------------
-- ADD REFERNCE EVENT ------
-- -------------------------
instance ToBSON AddReferenceEvent where
  toBSON (AddResourcePageReferenceEvent' event) = toBSON event
  toBSON (AddURLReferenceEvent' event) = toBSON event
  toBSON (AddCrossReferenceEvent' event) = toBSON event

instance FromBSON AddReferenceEvent where
  fromBSON doc = do
    referenceType <- BSON.lookup "referenceType" doc
    case referenceType of
      "ResourcePageReference" ->
        AddResourcePageReferenceEvent' <$> (fromBSON doc :: Maybe AddResourcePageReferenceEvent)
      "URLReference" -> AddURLReferenceEvent' <$> (fromBSON doc :: Maybe AddURLReferenceEvent)
      "CrossReference" -> AddCrossReferenceEvent' <$> (fromBSON doc :: Maybe AddCrossReferenceEvent)

-- ------------------------------------------------
instance ToBSON AddResourcePageReferenceEvent where
  toBSON model =
    [ "eventType" BSON.=: "AddReferenceEvent"
    , "referenceType" BSON.=: "ResourcePageReference"
    , "uuid" BSON.=: (model ^. uuid)
    , "path" BSON.=: (model ^. path)
    , "referenceUuid" BSON.=: (model ^. referenceUuid)
    , "shortUuid" BSON.=: (model ^. shortUuid)
    ]

instance FromBSON AddResourcePageReferenceEvent where
  fromBSON doc = do
    refUuid <- BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc
    refShortUuid <- BSON.lookup "shortUuid" doc
    return
      AddResourcePageReferenceEvent
      { _addResourcePageReferenceEventUuid = refUuid
      , _addResourcePageReferenceEventPath = refPath
      , _addResourcePageReferenceEventReferenceUuid = refReferenceUuid
      , _addResourcePageReferenceEventShortUuid = refShortUuid
      }

-- ------------------------------------------------
instance ToBSON AddURLReferenceEvent where
  toBSON model =
    [ "eventType" BSON.=: "AddReferenceEvent"
    , "referenceType" BSON.=: "URLReference"
    , "uuid" BSON.=: (model ^. uuid)
    , "path" BSON.=: (model ^. path)
    , "referenceUuid" BSON.=: (model ^. referenceUuid)
    , "url" BSON.=: (model ^. url)
    , "label" BSON.=: (model ^. label)
    ]

instance FromBSON AddURLReferenceEvent where
  fromBSON doc = do
    refUuid <- BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc
    refUrl <- BSON.lookup "url" doc
    refLabel <- BSON.lookup "label" doc
    return
      AddURLReferenceEvent
      { _addURLReferenceEventUuid = refUuid
      , _addURLReferenceEventPath = refPath
      , _addURLReferenceEventReferenceUuid = refReferenceUuid
      , _addURLReferenceEventUrl = refUrl
      , _addURLReferenceEventLabel = refLabel
      }

-- ------------------------------------------------
instance ToBSON AddCrossReferenceEvent where
  toBSON model =
    [ "eventType" BSON.=: "AddReferenceEvent"
    , "referenceType" BSON.=: "CrossReference"
    , "uuid" BSON.=: (model ^. uuid)
    , "path" BSON.=: (model ^. path)
    , "referenceUuid" BSON.=: (model ^. referenceUuid)
    , "targetUuid" BSON.=: (model ^. targetUuid)
    , "description" BSON.=: (model ^. description)
    ]

instance FromBSON AddCrossReferenceEvent where
  fromBSON doc = do
    refUuid <- BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc
    refTargetUuid <- BSON.lookup "targetUuid" doc
    refDescription <- BSON.lookup "description" doc
    return
      AddCrossReferenceEvent
      { _addCrossReferenceEventUuid = refUuid
      , _addCrossReferenceEventPath = refPath
      , _addCrossReferenceEventReferenceUuid = refReferenceUuid
      , _addCrossReferenceEventTargetUuid = refTargetUuid
      , _addCrossReferenceEventDescription = refDescription
      }

-- -------------------------
-- EDIT REFERNCE EVENT -----
-- -------------------------
instance ToBSON EditReferenceEvent where
  toBSON (EditResourcePageReferenceEvent' event) = toBSON event
  toBSON (EditURLReferenceEvent' event) = toBSON event
  toBSON (EditCrossReferenceEvent' event) = toBSON event

instance FromBSON EditReferenceEvent where
  fromBSON doc = do
    referenceType <- BSON.lookup "referenceType" doc
    case referenceType of
      "ResourcePageReference" ->
        EditResourcePageReferenceEvent' <$> (fromBSON doc :: Maybe EditResourcePageReferenceEvent)
      "URLReference" -> EditURLReferenceEvent' <$> (fromBSON doc :: Maybe EditURLReferenceEvent)
      "CrossReference" -> EditCrossReferenceEvent' <$> (fromBSON doc :: Maybe EditCrossReferenceEvent)

-- ------------------------------------------------
instance ToBSON EditResourcePageReferenceEvent where
  toBSON model =
    [ "eventType" BSON.=: "EditReferenceEvent"
    , "referenceType" BSON.=: "ResourcePageReference"
    , "uuid" BSON.=: (model ^. uuid)
    , "path" BSON.=: (model ^. path)
    , "referenceUuid" BSON.=: (model ^. referenceUuid)
    , "shortUuid" BSON.=: (model ^. shortUuid)
    ]

instance FromBSON EditResourcePageReferenceEvent where
  fromBSON doc = do
    refUuid <- BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc
    refShortUuid <- BSON.lookup "shortUuid" doc
    return
      EditResourcePageReferenceEvent
      { _editResourcePageReferenceEventUuid = refUuid
      , _editResourcePageReferenceEventPath = refPath
      , _editResourcePageReferenceEventReferenceUuid = refReferenceUuid
      , _editResourcePageReferenceEventShortUuid = refShortUuid
      }

-- ------------------------------------------------
instance ToBSON EditURLReferenceEvent where
  toBSON model =
    [ "eventType" BSON.=: "EditReferenceEvent"
    , "referenceType" BSON.=: "URLReference"
    , "uuid" BSON.=: (model ^. uuid)
    , "path" BSON.=: (model ^. path)
    , "referenceUuid" BSON.=: (model ^. referenceUuid)
    , "url" BSON.=: (model ^. url)
    , "label" BSON.=: (model ^. label)
    ]

instance FromBSON EditURLReferenceEvent where
  fromBSON doc = do
    refUuid <- BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc
    refUrl <- BSON.lookup "url" doc
    refLabel <- BSON.lookup "label" doc
    return
      EditURLReferenceEvent
      { _editURLReferenceEventUuid = refUuid
      , _editURLReferenceEventPath = refPath
      , _editURLReferenceEventReferenceUuid = refReferenceUuid
      , _editURLReferenceEventUrl = refUrl
      , _editURLReferenceEventLabel = refLabel
      }

-- ------------------------------------------------
instance ToBSON EditCrossReferenceEvent where
  toBSON model =
    [ "eventType" BSON.=: "EditReferenceEvent"
    , "referenceType" BSON.=: "CrossReference"
    , "uuid" BSON.=: (model ^. uuid)
    , "path" BSON.=: (model ^. path)
    , "referenceUuid" BSON.=: (model ^. referenceUuid)
    , "targetUuid" BSON.=: (model ^. targetUuid)
    , "description" BSON.=: (model ^. description)
    ]

instance FromBSON EditCrossReferenceEvent where
  fromBSON doc = do
    refUuid <- BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc
    refTargetUuid <- BSON.lookup "targetUuid" doc
    refDescription <- BSON.lookup "description" doc
    return
      EditCrossReferenceEvent
      { _editCrossReferenceEventUuid = refUuid
      , _editCrossReferenceEventPath = refPath
      , _editCrossReferenceEventReferenceUuid = refReferenceUuid
      , _editCrossReferenceEventTargetUuid = refTargetUuid
      , _editCrossReferenceEventDescription = refDescription
      }

-- -------------------------
-- DELETE REFERNCE EVENT ---
-- -------------------------
instance ToBSON DeleteReferenceEvent where
  toBSON model =
    [ "eventType" BSON.=: "DeleteReferenceEvent"
    , "uuid" BSON.=: (model ^. uuid)
    , "path" BSON.=: (model ^. path)
    , "referenceUuid" BSON.=: (model ^. referenceUuid)
    ]

instance FromBSON DeleteReferenceEvent where
  fromBSON doc = do
    refUuid <- BSON.lookup "uuid" doc
    refPath <- BSON.lookup "path" doc
    refReferenceUuid <- BSON.lookup "referenceUuid" doc
    return
      DeleteReferenceEvent
      { _deleteReferenceEventUuid = refUuid
      , _deleteReferenceEventPath = refPath
      , _deleteReferenceEventReferenceUuid = refReferenceUuid
      }
