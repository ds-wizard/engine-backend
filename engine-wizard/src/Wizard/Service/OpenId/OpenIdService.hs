module Wizard.Service.OpenId.OpenIdService where

import qualified Control.Exception.Base as E
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (FromJSON, Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (toLower)
import qualified Data.List as L
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Web.OIDC.Client as O
import qualified Web.OIDC.Client.IdTokenFlow as O_ID
import qualified Web.OIDC.Client.Tokens as OT

import Shared.Model.Error.Error
import Shared.Util.Crypto (generateRandomString)
import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Database.DAO.Common
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppHelper
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.User.UserService
import Wizard.Service.UserToken.UserTokenService
import Wizard.Util.Logger

createAuthenticationUrl :: String -> Maybe String -> Maybe String -> AppContextM ()
createAuthenticationUrl authId mFlow mClientUrl = do
  state <- liftIO $ generateRandomString 40
  let nonce = "FtEIbRdfFc7z2bNjCTaZKDcWNeUKUelvs13K21VL"
  (service, openIDClient) <- createOpenIDClient authId mClientUrl
  let params =
        fmap (\p -> (BS.pack p.name, Just . BS.pack $ p.value)) service.parameteres
          ++ [("nonce", Just . BS.pack $ nonce)]
  loc <-
    liftIO $
      case mFlow of
        Just "id_token" ->
          O_ID.getAuthenticationRequestUrl openIDClient [O.openId, O.email, O.profile] (Just . BS.pack $ state) params
        _ -> O.getAuthenticationRequestUrl openIDClient [O.openId, O.email, O.profile] (Just . BS.pack $ state) params
  throwError $ FoundError (show loc)

loginUser
  :: String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> AppContextM UserTokenDTO
loginUser authId mClientUrl mError mCode mNonce mIdToken mSessionState =
  runInTransaction $ do
    token <-
      case mIdToken of
        Just idToken -> return . parseToken $ idToken
        Nothing ->
          case mCode of
            Just code -> do
              httpClientManager <- asks httpClientManager
              (_, openIDClient) <- createOpenIDClient authId mClientUrl
              eTokens <-
                liftIO . E.try $ O.requestTokens openIDClient (fmap BS.pack mNonce) (BS.pack code) httpClientManager :: AppContextM (Either IOError (OT.Tokens Value))
              case eTokens of
                Right tokens -> return . O.idToken $ tokens
                Left error -> do
                  logWarnU _CMP_SERVICE . f' "Error in retrieving token from OpenID (error: '%s')" $ [show error]
                  throwError . UserError . _ERROR_VALIDATION__OPENID_WRONG_RESPONSE . show $ error
            Nothing -> throwError . UserError $ _ERROR_VALIDATION__OPENID_CODE_ABSENCE
    let claims = O.otherClaims token
    let mEmail = fmap (fmap toLower) . getClaim "email" $ claims
    let mFirstName = getClaim "given_name" claims
    let mLastName = getClaim "family_name" claims
    let mPicture = getClaim "picture" claims
    case (mEmail, mFirstName, mLastName) of
      (Just email, Just firstName, Just lastName) -> do
        user <- createUserFromExternalService authId firstName lastName email mPicture
        createToken user mSessionState
      _ -> throwError . UserError $ _ERROR_VALIDATION__OPENID_PROFILE_INFO_ABSENCE

parseToken :: FromJSON a => String -> O.IdTokenClaims a
parseToken = fromJust . decode . BSL.pack

-- --------------------------------
-- PRIVATE
-- --------------------------------
createOpenIDClient :: String -> Maybe String -> AppContextM (AppConfigAuthExternalService, O.OIDC)
createOpenIDClient authId mClientUrl = do
  httpClientManager <- asks httpClientManager
  serverConfig <- asks serverConfig
  appConfig <- getAppConfig
  clientUrl <- getAppClientUrl
  case L.find (\s -> s.aId == authId) appConfig.authentication.external.services of
    Just service -> do
      prov <- liftIO $ O.discover (T.pack service.url) httpClientManager
      let cId = BS.pack service.clientId
      let cSecret = BS.pack service.clientSecret
      let clientCallbackUrl = fromMaybe clientUrl mClientUrl
      let redirectUrl = BS.pack $ clientCallbackUrl ++ "/auth/" ++ authId ++ "/callback"
      let openIDClient = O.setCredentials cId cSecret redirectUrl (O.newOIDC prov)
      return (service, openIDClient)
    Nothing -> throwError . UserError $ _ERROR_SERVICE_AUTH__SERVICE_NOT_DEFINED authId

getClaim :: String -> Value -> Maybe String
getClaim key (Object obj) =
  case KM.lookup (fromString key) obj of
    Just (String string) -> Just . T.unpack $ string
    _ -> Nothing
getClaim _ _ = Nothing
