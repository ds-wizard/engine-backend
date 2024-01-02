module Registry.Api.Sentry where

import Data.Aeson (Value (..))

getSentryIdentity :: Maybe String -> [(String, Value)]
getSentryIdentity _ = []
