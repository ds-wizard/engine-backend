module Shared.Api.Resource.Event.TagEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.TagEventDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON AddTagEventDTO where
  parseJSON = simpleParseJSON "_addTagEventDTO"

instance ToJSON AddTagEventDTO where
  toJSON = simpleToJSON' "_addTagEventDTO" "eventType"

-- --------------------------------------------
instance FromJSON EditTagEventDTO where
  parseJSON = simpleParseJSON "_editTagEventDTO"

instance ToJSON EditTagEventDTO where
  toJSON = simpleToJSON' "_editTagEventDTO" "eventType"

-- --------------------------------------------
instance FromJSON DeleteTagEventDTO where
  parseJSON = simpleParseJSON "_deleteTagEventDTO"

instance ToJSON DeleteTagEventDTO where
  toJSON = simpleToJSON' "_deleteTagEventDTO" "eventType"
