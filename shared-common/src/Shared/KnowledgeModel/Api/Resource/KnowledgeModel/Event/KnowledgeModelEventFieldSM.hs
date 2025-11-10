module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM where

import Data.Swagger

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance ToSchema a => ToSchema (EventField a) where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
