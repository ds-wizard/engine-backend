module Shared.Api.Resource.Event.EventFieldSM where

import Data.Swagger

import Shared.Model.Event.EventField

instance ToSchema a => ToSchema (EventField a) where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
