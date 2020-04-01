module Shared.Api.Resource.Event.ChapterEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.ChapterEventDTO
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON AddChapterEventDTO where
  parseJSON = simpleParseJSON "_addChapterEventDTO"

instance ToJSON AddChapterEventDTO where
  toJSON = simpleToJSON' "_addChapterEventDTO" "eventType"

instance FromJSON EditChapterEventDTO where
  parseJSON = simpleParseJSON "_editChapterEventDTO"

instance ToJSON EditChapterEventDTO where
  toJSON = simpleToJSON' "_editChapterEventDTO" "eventType"

instance FromJSON DeleteChapterEventDTO where
  parseJSON = simpleParseJSON "_deleteChapterEventDTO"

instance ToJSON DeleteChapterEventDTO where
  toJSON = simpleToJSON' "_deleteChapterEventDTO" "eventType"
