module Shared.OpenId.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.OpenId.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlDTO
import Shared.OpenId.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlJM ()

instance ToSchema OpenIdClientAuthenticationUrlDTO where
  declareNamedSchema =
    toSwagger
      OpenIdClientAuthenticationUrlDTO
        { url = "https://idp.example.com/authorize?client_id=my-client&state=someState&nonce=someNonce"
        , state = "someState"
        }
