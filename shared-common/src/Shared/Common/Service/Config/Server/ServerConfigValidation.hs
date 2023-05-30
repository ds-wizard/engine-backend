module Shared.Common.Service.Config.Server.ServerConfigValidation where

import qualified Crypto.PubKey.RSA as RSA
import GHC.Records

import Shared.Common.Constant.DummyRsaPrivateKey
import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Error.Error

validateGeneralServerPort :: (HasField "general" serverConfig serverConfigGeneral, HasField "serverPort" serverConfigGeneral Int) => serverConfig -> Either AppError serverConfig
validateGeneralServerPort config
  | port < 1 || port > 65535 = Left . GeneralServerError $ _ERROR_SERVICE_CONFIG__VALIDATION_SERVER_PORT
  | otherwise = Right config
  where
    port = config.general.serverPort

validateGeneralSecret :: (HasField "general" serverConfig serverConfigGeneral, HasField "secret" serverConfigGeneral String) => serverConfig -> Either AppError serverConfig
validateGeneralSecret config
  | secretLen /= 32 = Left . GeneralServerError $ _ERROR_SERVICE_CONFIG__VALIDATION_SECRET
  | otherwise = Right config
  where
    secretLen = length $ config.general.secret

validateGeneralRsaPrivateKey :: (HasField "general" serverConfig serverConfigGeneral, HasField "rsaPrivateKey" serverConfigGeneral RSA.PrivateKey) => serverConfig -> Either AppError serverConfig
validateGeneralRsaPrivateKey config =
  if config.general.rsaPrivateKey == dummyRsaPrivateKey
    then Left . GeneralServerError $ _ERROR_SERVICE_CONFIG__VALIDATION_RSA_PRIVATE_KEY_FORMAT
    else Right config
