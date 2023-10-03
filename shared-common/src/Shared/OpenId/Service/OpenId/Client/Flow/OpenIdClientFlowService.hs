module Shared.OpenId.Service.OpenId.Client.Flow.OpenIdClientFlowService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.UUID as U
import qualified Web.OIDC.Client as O
import qualified Web.OIDC.Client.IdTokenFlow as O_ID

import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Crypto (generateRandomString)
import Shared.Common.Util.Maybe (concatMaybe)
import Shared.OpenId.Localization.Messages.Public
import Shared.OpenId.Model.OpenId.OpenIdClientParameter
import Shared.OpenId.Service.OpenId.Client.Flow.OpenIdClientFlowUtil

createAuthenticationUrl' :: AppContextC s sc m => O.OIDC -> [OpenIdClientParameter] -> Maybe String -> Maybe String -> m ()
createAuthenticationUrl' openIDClient parameters mFlow mClientUrl = do
  state <- liftIO $ generateRandomString 40
  let nonce = "FtEIbRdfFc7z2bNjCTaZKDcWNeUKUelvs13K21VL"
  let params = fmap (\p -> (BS.pack p.name, Just . BS.pack $ p.value)) parameters ++ [("nonce", Just . BS.pack $ nonce)]
  loc <-
    case mFlow of
      Just "id_token" -> liftIO $ O_ID.getAuthenticationRequestUrl openIDClient [O.openId, O.email, O.profile] (Just . BS.pack $ state) params
      _ -> liftIO $ O.getAuthenticationRequestUrl openIDClient [O.openId, O.email, O.profile] (Just . BS.pack $ state) params
  throwError $ FoundError (show loc)

getUserInfoFromOpenId :: AppContextC s sc m => O.OIDC -> Maybe String -> Maybe String -> Maybe String -> m (String, String, String, Maybe String, Maybe U.UUID)
getUserInfoFromOpenId openIDClient mCode mNonce mIdToken = do
  token <- parseIdToken openIDClient mCode mNonce mIdToken
  let claims = O.otherClaims token
  let mEmail = fmap (fmap toLower) . getClaim "email" $ claims
  let mFirstName = getClaim "given_name" claims
  let mLastName = getClaim "family_name" claims
  let mPicture = getClaim "picture" claims
  let mUserUuid = concatMaybe . fmap U.fromString . getClaim "user_uuid" $ claims
  case (mEmail, mFirstName, mLastName) of
    (Just email, Just firstName, Just lastName) -> return (email, firstName, lastName, mPicture, mUserUuid)
    _ -> throwError . UserError $ _ERROR_VALIDATION__OPENID_PROFILE_INFO_ABSENCE
