module Wizard.Api.Resource.Event.ChapterEventJM where

import Data.Aeson

import Wizard.Api.Resource.Event.ChapterEventDTO
import Wizard.Api.Resource.Event.EventFieldJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON')

instance FromJSON AddChapterEventDTO where
  parseJSON = simpleParseJSON "_addChapterEventDTO"

instance ToJSON AddChapterEventDTO where
  toJSON = simpleToJSON' "eventType" "_addChapterEventDTO"

instance FromJSON EditChapterEventDTO where
  parseJSON = simpleParseJSON "_editChapterEventDTO"

instance ToJSON EditChapterEventDTO where
  toJSON = simpleToJSON' "eventType" "_editChapterEventDTO"

instance FromJSON DeleteChapterEventDTO where
  parseJSON = simpleParseJSON "_deleteChapterEventDTO"

instance ToJSON DeleteChapterEventDTO where
  toJSON = simpleToJSON' "eventType" "_deleteChapterEventDTO"
