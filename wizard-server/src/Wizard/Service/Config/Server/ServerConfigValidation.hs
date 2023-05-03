module Wizard.Service.Config.Server.ServerConfigValidation where

import Shared.Common.Model.Error.Error
import Shared.Common.Service.Config.Server.ServerConfigValidation
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigIM ()
import Wizard.Model.Config.ServerConfigJM ()

validateServerConfig :: ServerConfig -> Either AppError ServerConfig
validateServerConfig config = do
  validateGeneralServerPort config
  validateGeneralSecret config
  validateGeneralRsaPrivateKey config
