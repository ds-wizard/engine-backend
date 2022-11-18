module Shared.Api.Resource.Event.ChapterEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Util.Aeson

instance FromJSON AddChapterEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddChapterEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON EditChapterEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditChapterEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

instance FromJSON DeleteChapterEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON DeleteChapterEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
