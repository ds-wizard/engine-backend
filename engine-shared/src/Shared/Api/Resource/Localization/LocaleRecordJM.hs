module Shared.Api.Resource.Localization.LocaleRecordJM where

import Data.Aeson

import Shared.Model.Localization.LocaleRecord
import Shared.Util.JSON

instance ToJSON LocaleRecord where
  toJSON = simpleToJSON "_localeRecord"

instance FromJSON LocaleRecord where
  parseJSON = simpleParseJSON "_localeRecord"
