module Wizard.Api.Resource.BookReference.BookReferenceJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.BookReference.BookReference

instance FromJSON BookReference where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BookReference where
  toJSON = genericToJSON jsonOptions
