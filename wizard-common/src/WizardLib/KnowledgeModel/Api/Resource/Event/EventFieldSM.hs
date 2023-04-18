module WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM where

import Data.Swagger

import WizardLib.KnowledgeModel.Model.Event.EventField

instance ToSchema a => ToSchema (EventField a) where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
