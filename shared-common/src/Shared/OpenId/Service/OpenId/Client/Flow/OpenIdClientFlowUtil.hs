module Shared.OpenId.Service.OpenId.Client.Flow.OpenIdClientFlowUtil where

import qualified Control.Exception.Base as E
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, liftIO)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Web.OIDC.Client as O
import qualified Web.OIDC.Client.Tokens as OT

import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.OpenId.Localization.Messages.Public

parseIdToken :: AppContextC s sc m => O.OIDC -> Maybe String -> Maybe String -> Maybe String -> m (OT.IdTokenClaims Value)
parseIdToken openIDClient mCode mNonce mIdToken =
  case mIdToken of
    Just idToken -> return . fromJust . decode . BSL.pack $ idToken
    Nothing ->
      case mCode of
        Just code -> do
          context <- ask
          eTokens <- liftIO . E.try $ O.requestTokens openIDClient (fmap BS.pack mNonce) (BS.pack code) context.httpClientManager' :: AppContextC s sc m => m (Either IOError (OT.Tokens Value))
          case eTokens of
            Right tokens -> return . O.idToken $ tokens
            Left error -> do
              logWarnI _CMP_SERVICE . f' "Error in retrieving token from OpenID (error: '%s')" $ [show error]
              throwError . UserError . _ERROR_VALIDATION__OPENID_WRONG_RESPONSE . show $ error
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__OPENID_CODE_ABSENCE

getClaim :: String -> Value -> Maybe String
getClaim key (Object obj) =
  case KM.lookup (fromString key) obj of
    Just (String string) -> Just . T.unpack $ string
    _ -> Nothing
getClaim _ _ = Nothing
