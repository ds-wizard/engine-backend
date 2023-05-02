module Shared.Common.Localization.Locale where

import qualified Data.Map.Strict as M

import Shared.Common.Model.Localization.LocaleRecord
import Shared.Common.Util.String (f')

locale :: M.Map String String -> LocaleRecord -> String
locale ls (LocaleRecord key defaultValue variables) =
  case M.lookup key ls of
    Just value -> f' value variables
    Nothing -> f' defaultValue variables
