module Shared.Common.Api.Resource.Common.AesonSM where

import Data.Aeson
import Data.Swagger

instance ToSchema Value where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Value") mempty
