module WizardLib.KnowledgeModel.Api.Resource.Event.ChapterEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent

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
