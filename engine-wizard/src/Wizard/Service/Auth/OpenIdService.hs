module Wizard.Service.Auth.OpenIdService where

import Control.Lens ((^.), (^?))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Web.OIDC.Client as O
import qualified Web.OIDC.Client.Tokens as OT

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Crypto (generateRandomString)
import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Database.DAO.Common
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Token.TokenService
import Wizard.Service.User.UserService

createAuthenticationUrl :: String -> Maybe String -> AppContextM ()
createAuthenticationUrl authId mClientUrl =
  runInTransaction $ do
    state <- liftIO $ generateRandomString 40
    (service, openIDClient) <- createOpenIDClient authId mClientUrl
    let params = fmap (\p -> (BS.pack (p ^. name), Just . BS.pack $ (p ^. value))) (service ^. parameteres)
    loc <-
      liftIO $ O.getAuthenticationRequestUrl openIDClient [O.openId, O.email, O.profile] (Just . BS.pack $ state) params
    throwError $ FoundError (show loc)

loginUser :: String -> Maybe String -> Maybe String -> Maybe String -> AppContextM TokenDTO
loginUser authId mClientUrl mError mCode =
  runInTransaction $
  case mCode of
    Just code -> do
      httpClientManager <- asks _appContextHttpClientManager
      (_, openIDClient) <- createOpenIDClient authId mClientUrl
      tokens <-
        liftIO $ O.requestTokens openIDClient Nothing (BS.pack code) httpClientManager :: AppContextM (OT.Tokens Value)
      let claims = O.otherClaims . O.idToken $ tokens
      let mEmail = fmap toLower . T.unpack <$> (claims ^? key "email" . _String)
      let mFirstName = T.unpack <$> claims ^? key "given_name" . _String
      let mLastName = T.unpack <$> claims ^? key "family_name" . _String
      let mPicture = T.unpack <$> claims ^? key "picture" . _String
      case (mEmail, mFirstName, mLastName) of
        (Just email, Just firstName, Just lastName) -> do
          userDto <- createUserFromExternalService authId firstName lastName email mPicture
          generateTokenFromUser userDto
        _ -> throwError . UserError $ _ERROR_VALIDATION__OPENID_PROFILE_INFO_ABSENCE
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__OPENID_CODE_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
createOpenIDClient :: String -> Maybe String -> AppContextM (AppConfigAuthExternalService, O.OIDC)
createOpenIDClient authId mClientUrl = do
  httpClientManager <- asks _appContextHttpClientManager
  serverConfig <- asks _appContextServerConfig
  appConfig <- getAppConfig
  case L.find (\s -> s ^. aId == authId) (appConfig ^. authentication . external . services) of
    Just service -> do
      prov <- liftIO $ O.discover (T.pack $ service ^. url) httpClientManager
      let cId = BS.pack $ service ^. clientId
      let cSecret = BS.pack $ service ^. clientSecret
      let clientCallbackUrl = fromMaybe (serverConfig ^. general . clientUrl) mClientUrl
      let redirectUrl = BS.pack $ clientCallbackUrl ++ "/auth/" ++ authId ++ "/callback"
      let openIDClient = O.setCredentials cId cSecret redirectUrl (O.newOIDC prov)
      return (service, openIDClient)
    Nothing -> throwError . UserError $ _ERROR_SERVICE_AUTH__SERVICE_NOT_DEFINED authId
