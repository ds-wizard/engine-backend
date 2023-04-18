module Shared.Common.Api.Resource.Localization.LocaleRecordJM where

import Data.Aeson

import Shared.Common.Model.Localization.LocaleRecord
import Shared.Common.Util.Aeson

instance ToJSON LocaleRecord where
  toJSON = genericToJSON jsonOptions

instance FromJSON LocaleRecord where
  parseJSON = genericParseJSON jsonOptions
