module Wizard.Integration.Http.Admin.Runner where

import Control.Monad.Reader (asks)
import qualified Jose.Jwk as JWK

import Wizard.Integration.Http.Admin.RequestMapper
import Wizard.Integration.Http.Admin.ResponseMapper
import Wizard.Integration.Http.Common.HttpClient
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext

retrieveJwtPublicKeys :: AppContextM JWK.JwkSet
retrieveJwtPublicKeys = do
  serverConfig <- asks serverConfig
  runRequest (toRetrieveJwtPublicKeysRequest serverConfig.admin) toRetrieveJwtPublicKeysResponse
