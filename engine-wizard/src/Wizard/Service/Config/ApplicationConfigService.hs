module Wizard.Service.Config.ApplicationConfigService where

import Control.Lens (Lens', (&), (.~), (^.))
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither)
import System.Environment (lookupEnv)

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigJM ()

getApplicationConfig :: String -> IO (Either AppError AppConfig)
getApplicationConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return config >>= applyEnvVariable "FEEDBACK_TOKEN" (feedback . token) >>= (return . Right)
    Left error -> return . Left . GeneralServerError . show $ error
  where
    applyEnvVariable :: String -> Lens' AppConfig String -> AppConfig -> IO AppConfig
    applyEnvVariable envVariableName accessor config = do
      envVariable <- lookupEnv envVariableName
      let newValue = fromMaybe (config ^. accessor) envVariable
      return $ config & accessor .~ newValue
