module Wizard.Api.Sentry where

import Data.Aeson (Value (..), toJSON)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Shared.Common.Util.Token
import WizardLib.Public.Service.UserToken.UserTokenUtil

getSentryIdentity :: Maybe String -> [(String, Value)]
getSentryIdentity mAuthorizationHeader =
  [
    ( "sentry.interfaces.User"
    , toJSON $ HashMap.fromList [("id", String . T.pack . fromMaybe "" . getUserUuidFromToken . fromMaybe "" . separateToken . fromMaybe "" $ mAuthorizationHeader)]
    )
  ]
