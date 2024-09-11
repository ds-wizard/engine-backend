module Shared.OpenId.Service.OpenId.Client.Flow.OpenIdClientFlowUtil where

import Control.Monad.Except (throwError)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toLower)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Web.OIDC.Client as O
import qualified Web.OIDC.Client.Tokens as OT

import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Maybe (concatMaybe)
import Shared.OpenId.Localization.Messages.Public

parseIdToken :: AppContextC s sc m => OT.IdTokenClaims A.Value -> m (String, String, String, Maybe String, Maybe U.UUID)
parseIdToken idToken = do
  let claims = O.otherClaims idToken
  let mEmail = fmap (fmap toLower) . getClaim "email" $ claims
  let mFirstName = getClaim "given_name" claims
  let mLastName = getClaim "family_name" claims
  let mPicture = getClaim "picture" claims
  let mUserUuid = concatMaybe . fmap U.fromString . getClaim "user_uuid" $ claims
  case (mEmail, mFirstName, mLastName) of
    (Just email, Just firstName, Just lastName) -> return (email, firstName, lastName, mPicture, mUserUuid)
    _ -> throwError . UserError $ _ERROR_VALIDATION__OPENID_PROFILE_INFO_ABSENCE

getClaim :: String -> A.Value -> Maybe String
getClaim key (A.Object obj) =
  case KM.lookup (fromString key) obj of
    Just (A.String string) -> Just . T.unpack $ string
    _ -> Nothing
getClaim _ _ = Nothing
