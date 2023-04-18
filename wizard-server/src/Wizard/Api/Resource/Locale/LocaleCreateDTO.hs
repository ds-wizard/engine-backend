module Wizard.Api.Resource.Locale.LocaleCreateDTO where

import qualified Data.ByteString.Char8 as BS
import GHC.Generics

data LocaleCreateDTO = LocaleCreateDTO
  { name :: String
  , description :: String
  , code :: String
  , localeId :: String
  , version :: String
  , license :: String
  , readme :: String
  , recommendedAppVersion :: String
  , content :: BS.ByteString
  }
  deriving (Show, Eq, Generic)
