module Wizard.Database.Migration.Development.Locale.Data.Locales where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M

import Shared.Constant.App
import Shared.Util.Date
import Shared.Util.Uuid
import Wizard.Model.Locale.Locale

localeCz :: Locale
localeCz =
  Locale
    { _localeUuid = u' "b5f6ea5e-89c2-4419-930a-69980bbc36e8"
    , _localeName = "Čeština"
    , _localeShortcut = "cs"
    , _localeFallback = True
    , _localeEnabled = True
    , _localeAppUuid = defaultAppUuid
    , _localeCreatedAt = dt' 2022 1 21
    , _localeUpdatedAt = dt' 2022 1 21
    }

localeCzContent :: BS.ByteString
localeCzContent = BSL.toStrict . encode . M.insert "someKey" "someValue" $ M.empty
