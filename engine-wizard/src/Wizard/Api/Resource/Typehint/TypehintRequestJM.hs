module Wizard.Api.Resource.Typehint.TypehintRequestJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Typehint.TypehintRequestDTO

instance FromJSON TypehintRequestDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TypehintRequestDTO where
  toJSON = genericToJSON simpleOptions
