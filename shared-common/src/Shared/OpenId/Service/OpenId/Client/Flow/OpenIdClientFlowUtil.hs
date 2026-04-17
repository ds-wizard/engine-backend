module Shared.OpenId.Service.OpenId.Client.Flow.OpenIdClientFlowUtil where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toLower)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Web.OIDC.Client as O
import qualified Web.OIDC.Client.Tokens as OT

import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Maybe (concatMaybe)

parseIdToken
  :: AppContextC s sc m
  => OT.IdTokenClaims A.Value
  -> m (Maybe String, Maybe String, Maybe String, Maybe String, Maybe U.UUID)
parseIdToken idToken = do
  let claims = O.otherClaims idToken
  let mEmail = fmap (fmap toLower) . getClaim "email" $ claims
  let mFirstName = getClaim "given_name" claims
  let mLastName = getClaim "family_name" claims
  let mPicture = getClaim "picture" claims
  let mUserUuid = concatMaybe . fmap U.fromString . getClaim "user_uuid" $ claims
  return (mEmail, mFirstName, mLastName, mPicture, mUserUuid)

getClaim :: String -> A.Value -> Maybe String
getClaim key (A.Object obj) =
  case KM.lookup (fromString key) obj of
    Just (A.String string) -> Just . T.unpack $ string
    _ -> Nothing
getClaim _ _ = Nothing
