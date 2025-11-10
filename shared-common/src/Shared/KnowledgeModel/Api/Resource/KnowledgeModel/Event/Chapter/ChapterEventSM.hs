module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Chapter.ChapterEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Chapter.ChapterEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent

instance ToSchema AddChapterEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1

instance ToSchema EditChapterEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1

instance ToSchema DeleteChapterEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1
