module Shared.Api.Resource.Event.ChapterEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.ChapterEventDTO
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Util.JSON

instance FromJSON AddChapterEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AddChapterEventDTO where
  toJSON = simpleToJSON' "_addChapterEventDTO" "eventType"

instance FromJSON EditChapterEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON EditChapterEventDTO where
  toJSON = simpleToJSON' "_editChapterEventDTO" "eventType"

instance FromJSON DeleteChapterEventDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DeleteChapterEventDTO where
  toJSON = simpleToJSON' "_deleteChapterEventDTO" "eventType"
