module WizardLib.KnowledgeModel.Api.Resource.Event.ChapterEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.ChapterEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent

instance ToSchema AddChapterEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1

instance ToSchema EditChapterEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1

instance ToSchema DeleteChapterEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1
