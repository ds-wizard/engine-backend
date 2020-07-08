module Shared.Api.Resource.Event.TagEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Tag.TagEvent
import Shared.Util.JSON

instance FromJSON AddTagEvent where
  parseJSON = simpleParseJSON "_addTagEvent"

instance ToJSON AddTagEvent where
  toJSON = simpleToJSON' "_addTagEvent" "eventType"

-- --------------------------------------------
instance FromJSON EditTagEvent where
  parseJSON = simpleParseJSON "_editTagEvent"

instance ToJSON EditTagEvent where
  toJSON = simpleToJSON' "_editTagEvent" "eventType"

-- --------------------------------------------
instance FromJSON DeleteTagEvent where
  parseJSON = simpleParseJSON "_deleteTagEvent"

instance ToJSON DeleteTagEvent where
  toJSON = simpleToJSON' "_deleteTagEvent" "eventType"
