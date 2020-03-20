module Shared.Api.Resource.Event.EventFieldSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldDTO

instance ToSchema a => ToSchema (EventFieldDTO a) where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
--  declareNamedSchema = genericDeclareNamedSchema
