module Shared.Api.Resource.Event.ChapterEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.ChapterEventJM ()
import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Chapter.ChapterEvent
import Shared.Util.Swagger

instance ToSchema AddChapterEvent where
  declareNamedSchema = simpleToSchema'' "_addChapterEvent" "eventType" a_km1_ch1

instance ToSchema EditChapterEvent where
  declareNamedSchema = simpleToSchema'' "_editChapterEvent" "eventType" e_km1_ch1

instance ToSchema DeleteChapterEvent where
  declareNamedSchema = simpleToSchema'' "_deleteChapterEvent" "eventType" d_km1_ch1
