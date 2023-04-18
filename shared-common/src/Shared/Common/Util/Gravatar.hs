module Shared.Common.Util.Gravatar (
  createGravatarHash,
) where

import Shared.Common.Util.Crypto (hashMD5)
import Shared.Common.Util.String (toLower, trim)

type Email = String

createGravatarHash :: Email -> String
createGravatarHash = hashMD5 . toLower . trim
