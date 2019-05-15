module Service.Config.ApplicationConfigService where

import Control.Lens (Lens', (&), (.~), (^.))
import Control.Monad (msum)
import Data.Yaml (decodeFileEither)
import System.Environment (lookupEnv)

import LensesConfig
import Model.Config.AppConfig
import Model.Config.AppConfigJM ()
import Model.Error.Error

getApplicationConfig :: String -> IO (Either AppError AppConfig)
getApplicationConfig fileName = do
  eConfig <- decodeFileEither fileName
  case eConfig of
    Right config -> return config >>= applyEnvVariable "FEEDBACK_TOKEN" (feedback . token) >>= (return . Right)
    Left error -> return . Left . GeneralServerError . show $ error
  where
    applyEnvVariable :: String -> Lens' AppConfig (Maybe String) -> AppConfig -> IO AppConfig
    applyEnvVariable envVariableName accessor config = do
      envVariable <- lookupEnv envVariableName
      let newValue = msum [envVariable, config ^. accessor]
      return $ config & accessor .~ newValue
