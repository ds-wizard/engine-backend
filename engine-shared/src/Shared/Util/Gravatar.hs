module Shared.Util.Gravatar
  ( gravatarHash
  ) where

import Shared.Util.Crypto (hashMD5)
import Shared.Util.String (toLower, trim)

type Email = String

gravatarHash :: Email -> String
gravatarHash = hashMD5 . toLower . trim
