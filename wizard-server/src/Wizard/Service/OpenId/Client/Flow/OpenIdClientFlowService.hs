module Wizard.Service.OpenId.Client.Flow.OpenIdClientFlowService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Web.OIDC.Client as O

import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Model.Error.Error
import Shared.OpenId.Service.OpenId.Client.Flow.OpenIdClientFlowService
import Shared.OpenId.Service.OpenId.Client.Flow.OpenIdClientFlowUtil
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.ActionKey.ActionKeyType
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.ActionKey.ActionKeyService
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantHelper
import Wizard.Service.User.UserService
import Wizard.Service.User.UserUtil
import Wizard.Service.UserToken.Login.LoginService
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO

createAuthenticationUrl :: String -> Maybe String -> Maybe String -> AppContextM ()
createAuthenticationUrl authId mFlow mClientUrl = do
  (service, openIDClient) <- createOpenIDClient authId mClientUrl
  createAuthenticationUrl' openIDClient service.parameters mFlow mClientUrl

loginUser :: String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> AppContextM UserTokenDTO
loginUser authId mClientUrl mError mCode mNonce mIdToken mUserAgent mSessionState =
  runInTransaction $ do
    httpClientManager <- asks httpClientManager
    (_, openIDClient) <- createOpenIDClient authId mClientUrl
    idToken <-
      case mIdToken of
        Just idToken -> return . fromJust . A.decode . BSL.pack $ idToken
        Nothing -> do
          tokens <- requestTokensWithCode openIDClient mCode mNonce
          return . O.idToken $ tokens
    (email, firstName, lastName, mPicture, mUserUuid) <- parseIdToken idToken
    mUserFromDb <- findUserByEmail' email
    consentRequired <- isConsentRequired mUserFromDb
    user <- createUserFromExternalService mUserFromDb authId firstName lastName email mPicture mUserUuid (not consentRequired)
    case (mUserFromDb, consentRequired) of
      (Nothing, True) -> do
        actionKey <- createActionKey user.uuid ConsentsRequiredActionKey user.tenantUuid
        return $ ConsentsRequiredDTO {hash = actionKey.hash}
      _ -> createLoginToken user mUserAgent mSessionState

-- --------------------------------
-- PRIVATE
-- --------------------------------
createOpenIDClient :: String -> Maybe String -> AppContextM (TenantConfigAuthenticationExternalService, O.OIDC)
createOpenIDClient authId mClientUrl = do
  httpClientManager <- asks httpClientManager
  serverConfig <- asks serverConfig
  tcAuthentication <- getCurrentTenantConfigAuthentication
  clientUrl <- getClientUrl
  case L.find (\s -> s.aId == authId) tcAuthentication.external.services of
    Just service -> do
      prov <- liftIO $ O.discover (T.pack service.url) httpClientManager
      let cId = BS.pack service.clientId
      let cSecret = BS.pack service.clientSecret
      let clientCallbackUrl = fromMaybe clientUrl mClientUrl
      let redirectUrl = BS.pack $ clientCallbackUrl ++ "/auth/" ++ authId ++ "/callback"
      let openIDClient = O.setCredentials cId cSecret redirectUrl (O.newOIDC prov)
      return (service, openIDClient)
    Nothing -> throwError . UserError $ _ERROR_SERVICE_AUTH__SERVICE_NOT_DEFINED authId
