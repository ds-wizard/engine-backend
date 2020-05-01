module Shared.Api.Resource.Event.TagEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.TagEventDTO
import Shared.Util.JSON

instance FromJSON AddTagEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AddTagEventDTO where
  toJSON = simpleToJSON' "_addTagEventDTO" "eventType"

-- --------------------------------------------
instance FromJSON EditTagEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON EditTagEventDTO where
  toJSON = simpleToJSON' "_editTagEventDTO" "eventType"

-- --------------------------------------------
instance FromJSON DeleteTagEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DeleteTagEventDTO where
  toJSON = simpleToJSON' "_deleteTagEventDTO" "eventType"
