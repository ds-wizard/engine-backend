module Wizard.Api.Resource.Event.TagEventJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON')
import Wizard.Api.Resource.Event.EventFieldJM ()
import Wizard.Api.Resource.Event.TagEventDTO

instance FromJSON AddTagEventDTO where
  parseJSON = simpleParseJSON "_addTagEventDTO"

instance ToJSON AddTagEventDTO where
  toJSON = simpleToJSON' "eventType" "_addTagEventDTO"

-- --------------------------------------------
instance FromJSON EditTagEventDTO where
  parseJSON = simpleParseJSON "_editTagEventDTO"

instance ToJSON EditTagEventDTO where
  toJSON = simpleToJSON' "eventType" "_editTagEventDTO"

-- --------------------------------------------
instance FromJSON DeleteTagEventDTO where
  parseJSON = simpleParseJSON "_deleteTagEventDTO"

instance ToJSON DeleteTagEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteTagEventDTO"
