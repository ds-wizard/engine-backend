module Shared.Util.Gravatar
  ( createGravatarHash
  ) where

import Shared.Util.Crypto (hashMD5)
import Shared.Util.String (toLower, trim)

type Email = String

createGravatarHash :: Email -> String
createGravatarHash = hashMD5 . toLower . trim
