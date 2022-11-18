module Shared.Api.Resource.Localization.LocaleRecordJM where

import Data.Aeson

import Shared.Model.Localization.LocaleRecord
import Shared.Util.Aeson

instance ToJSON LocaleRecord where
  toJSON = genericToJSON jsonOptions

instance FromJSON LocaleRecord where
  parseJSON = genericParseJSON jsonOptions
