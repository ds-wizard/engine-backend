module Wizard.Service.OpenId.Client.Flow.OpenIdClientFlowService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U
import qualified Web.OIDC.Client as O
import qualified Web.OIDC.Client.IdTokenFlow as O_ID
import qualified Web.OIDC.Client.Tokens as OT

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Crypto (generateRandomString)
import Shared.Common.Util.Uuid
import Shared.OpenId.Model.OpenId.OpenIdClientParameter
import Shared.OpenId.Service.OpenId.Client.Flow.OpenIdClientFlowService
import Shared.OpenId.Service.OpenId.Client.Flow.OpenIdClientFlowUtil (parseIdToken)
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Mapping.User.UserRegistrationPendingServiceType ()
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.User.User
import Wizard.Model.User.UserRegistrationPendingServiceType
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantHelper
import Wizard.Service.User.UserService
import Wizard.Service.User.UserUtil
import Wizard.Service.UserToken.Login.LoginService
import Wizard.Service.UserToken.Login.LoginValidation
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Database.DAO.OpenId.OpenIdClientDefinitionDAO
import WizardLib.Public.Database.DAO.User.UserOpenIdIdentityDAO
import WizardLib.Public.Localization.Messages.Public
import WizardLib.Public.Model.OpenId.OpenIdClient
import WizardLib.Public.Model.User.UserOpenIdIdentity
import WizardLib.Public.Model.User.UserRegistrationPending
import WizardLib.Public.Service.User.UserRegistrationPendingService (upsertPendingExternalRegistration)

createAuthenticationUrl :: U.UUID -> Maybe String -> Maybe String -> AppContextM ()
createAuthenticationUrl providerUuid mFlow mClientUrl = do
  (openIdClient, oidc) <- buildOidcClient providerUuid mClientUrl
  let scopes = buildScopes openIdClient
  state <- liftIO $ generateRandomString 40
  let nonce = "FtEIbRdfFc7z2bNjCTaZKDcWNeUKUelvs13K21VL"
  let params =
        fmap (\p -> (BS.pack p.name, Just . BS.pack $ p.value)) openIdClient.parameters
          ++ [("nonce", Just . BS.pack $ nonce)]
  loc <-
    case mFlow of
      Just "id_token" -> liftIO $ O_ID.getAuthenticationRequestUrl oidc scopes (Just . BS.pack $ state) params
      _ -> liftIO $ O.getAuthenticationRequestUrl oidc scopes (Just . BS.pack $ state) params
  throwError $ FoundError (show loc)

loginUserOrLinkIdentity
  :: Bool
  -> U.UUID
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> AppContextM UserTokenDTO
loginUserOrLinkIdentity isAuthenticated providerUuid mClientUrl mError mCode mNonce mIdToken mUserAgent mSessionState =
  if isAuthenticated
    then do
      mCurrentUserUuid <- getCurrentUserUuid
      case mCurrentUserUuid of
        Just currentUserUuid -> do
          linkOpenIdIdentity currentUserUuid providerUuid mClientUrl mError mCode mNonce mIdToken
          return IdentityLinkedDTO
        Nothing -> loginUser providerUuid mClientUrl mError mCode mNonce mIdToken mUserAgent mSessionState
    else loginUser providerUuid mClientUrl mError mCode mNonce mIdToken mUserAgent mSessionState

loginUser
  :: U.UUID
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> AppContextM UserTokenDTO
loginUser providerUuid mClientUrl _mError mCode mNonce mIdToken mUserAgent mSessionState =
  runInTransaction $ do
    (openIdClient, oidc) <- buildOidcClient providerUuid mClientUrl
    (externalId, mEmail, mFirstName, mLastName, mPicture, mUserUuid) <- resolveExternalIdentity oidc mCode mNonce mIdToken
    tcAuthentication <- getCurrentTenantConfigAuthentication
    mIdentity <- findUserOpenIdIdentityByExternalIdAndProvider' externalId providerUuid
    case mIdentity of
      Just identity -> do
        user <- findUserByUuid identity.userUuid
        validateLoginEnabled tcAuthentication user
        createLoginToken user mUserAgent mSessionState
      Nothing -> do
        mUserByEmail <- case mEmail of
          Just email -> findUserByEmail' email
          Nothing -> return Nothing
        case mUserByEmail of
          Just userByEmail -> do
            validateLoginEnabled tcAuthentication userByEmail
            insertOpenIdIdentityLink userByEmail.uuid openIdClient externalId
            createLoginToken userByEmail mUserAgent mSessionState
          Nothing -> do
            unless openIdClient.registrationEnabled $
              throwError $
                UserError _ERROR_SERVICE_OPENID__REGISTRATION_DISABLED
            case (mEmail, mFirstName, mLastName) of
              (Just email, Just firstName, Just lastName) -> do
                consentRequired <- isConsentRequired Nothing
                user <- createUserFromOpenIdLogin openIdClient externalId firstName lastName email mPicture mUserUuid (not consentRequired)
                validateLoginEnabled tcAuthentication user
                createLoginToken user mUserAgent mSessionState
              _ -> do
                pending <- upsertPendingExternalRegistration OpenIdUserRegistrationPendingServiceType providerUuid externalId Nothing mEmail mFirstName mLastName mPicture Nothing
                return $
                  CompleteRegistrationRequiredDTO
                    { hash = pending.hash
                    , email = pending.email
                    , firstName = pending.firstName
                    , lastName = pending.lastName
                    , imageUrl = pending.imageUrl
                    }

linkOpenIdIdentity
  :: U.UUID
  -> U.UUID
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> AppContextM ()
linkOpenIdIdentity currentUserUuid providerUuid mClientUrl _mError mCode mNonce mIdToken =
  runInTransaction $ do
    (openIdClient, oidc) <- buildOidcClient providerUuid mClientUrl
    (externalId, _mEmail, _mFirstName, _mLastName, _mPicture, _mUserUuid) <- resolveExternalIdentity oidc mCode mNonce mIdToken
    mIdentity <- findUserOpenIdIdentityByExternalIdAndProvider' externalId providerUuid
    case mIdentity of
      Just identity ->
        unless (identity.userUuid == currentUserUuid) $
          throwError $
            UserError _ERROR_SERVICE_OPENID__IDENTITY_LINKED_TO_DIFFERENT_USER
      Nothing -> insertOpenIdIdentityLink currentUserUuid openIdClient externalId

-- --------------------------------
-- PRIVATE
-- --------------------------------
buildOidcClient :: U.UUID -> Maybe String -> AppContextM (OpenIdClient, O.OIDC)
buildOidcClient providerUuid mClientUrl = do
  httpClientManager <- asks httpClientManager
  clientUrl <- getClientUrl
  mOpenIdClient <- findOpenIdClientDefinitionByUuid' providerUuid
  case mOpenIdClient of
    Just openIdClient -> do
      prov <- liftIO $ O.discover (T.pack openIdClient.url) httpClientManager
      let cId = BS.pack openIdClient.clientId
      let cSecret = BS.pack openIdClient.clientSecret
      let clientCallbackUrl = fromMaybe clientUrl mClientUrl
      let redirectUrl = BS.pack $ clientCallbackUrl ++ "/open-id/" ++ U.toString providerUuid ++ "/callback"
      let oidc = O.setCredentials cId cSecret redirectUrl (O.newOIDC prov)
      return (openIdClient, oidc)
    Nothing -> throwError . UserError $ _ERROR_SERVICE_AUTH__SERVICE_NOT_DEFINED (U.toString providerUuid)

buildScopes :: OpenIdClient -> [O.ScopeValue]
buildScopes openIdClient =
  [O.openId]
    ++ [O.email | openIdClient.scopeEmail]
    ++ [O.profile | openIdClient.scopeProfile]

resolveExternalIdentity
  :: O.OIDC
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> AppContextM (String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe U.UUID)
resolveExternalIdentity oidc mCode mNonce mIdToken = do
  idToken <-
    case mIdToken of
      Just idToken -> return . fromJust . A.decode . BSL.pack $ idToken
      Nothing -> do
        tokens <- requestTokensWithCode oidc mCode mNonce
        return . O.idToken $ tokens
  let externalId = T.unpack . OT.sub $ idToken
  (mEmail, mFirstName, mLastName, mPicture, mUserUuid) <- parseIdToken idToken
  return (externalId, mEmail, mFirstName, mLastName, mPicture, mUserUuid)

insertOpenIdIdentityLink :: U.UUID -> OpenIdClient -> String -> AppContextM ()
insertOpenIdIdentityLink userUuid openIdClient externalId = do
  identityUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let identity =
        UserOpenIdIdentity
          { uuid = identityUuid
          , externalId = externalId
          , externalLabel = Nothing
          , userUuid = userUuid
          , providerUuid = openIdClient.uuid
          , tenantUuid = openIdClient.tenantUuid
          , createdAt = now
          }
  _ <- insertUserOpenIdIdentity identity
  return ()
