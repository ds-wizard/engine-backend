module Shared.Api.Resource.Event.ChapterEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.ChapterEventDTO
import Shared.Api.Resource.Event.ChapterEventJM ()
import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Service.Event.EventToDTO
import Shared.Util.Swagger

instance ToSchema AddChapterEventDTO where
  declareNamedSchema = simpleToSchema'' "_addChapterEventDTO" "eventType" (toDTO a_km1_ch1)

instance ToSchema EditChapterEventDTO where
  declareNamedSchema = simpleToSchema'' "_editChapterEventDTO" "eventType" (toDTO e_km1_ch1)

instance ToSchema DeleteChapterEventDTO where
  declareNamedSchema = simpleToSchema'' "_deleteChapterEventDTO" "eventType" (toDTO d_km1_ch1)
