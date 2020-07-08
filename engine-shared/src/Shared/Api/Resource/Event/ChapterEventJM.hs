module Shared.Api.Resource.Event.ChapterEventJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Util.JSON

instance FromJSON AddChapterEvent where
  parseJSON = simpleParseJSON "_addChapterEvent"

instance ToJSON AddChapterEvent where
  toJSON = simpleToJSON' "_addChapterEvent" "eventType"

instance FromJSON EditChapterEvent where
  parseJSON = simpleParseJSON "_editChapterEvent"

instance ToJSON EditChapterEvent where
  toJSON = simpleToJSON' "_editChapterEvent" "eventType"

instance FromJSON DeleteChapterEvent where
  parseJSON = simpleParseJSON "_deleteChapterEvent"

instance ToJSON DeleteChapterEvent where
  toJSON = simpleToJSON' "_deleteChapterEvent" "eventType"
