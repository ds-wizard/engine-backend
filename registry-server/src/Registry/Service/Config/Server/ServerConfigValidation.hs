module Registry.Service.Config.Server.ServerConfigValidation where

import Registry.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Shared.Common.Service.Config.Server.ServerConfigValidation

validateServerConfig :: ServerConfig -> Either AppError ServerConfig
validateServerConfig config = do
  validateGeneralServerPort config
