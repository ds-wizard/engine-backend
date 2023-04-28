module Wizard.Integration.Http.Admin.ResponseMapper where

import qualified Data.ByteString.Lazy as BSL
import qualified Jose.Jwk as JWK
import Network.HTTP.Client (Response)

import Shared.Common.Model.Error.Error
import Wizard.Integration.Http.Common.ResponseMapper

toRetrieveJwtPublicKeysResponse :: Response BSL.ByteString -> Either AppError JWK.JwkSet
toRetrieveJwtPublicKeysResponse = extractResponseBody
