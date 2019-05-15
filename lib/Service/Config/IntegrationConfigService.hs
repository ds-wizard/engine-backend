module Service.Config.IntegrationConfigService
  ( getIntegrationConfig
  -- Helpers
  , heGetIntegrationConfig
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither)

import LensesConfig hiding (items)
import Model.Context.AppContext
import Model.Error.Error

getIntegrationConfig :: String -> AppContextM (Either AppError (M.Map String String))
getIntegrationConfig sectionName = do
  appConfig <- asks _appContextAppConfig
  let integrationConfigFileName = appConfig ^. general . integrationConfig
  eIntConfig <- liftIO $ decodeFileEither ("config/" ++ integrationConfigFileName)
  case eIntConfig of
    Right intConfig -> return . Right . fromMaybe M.empty . M.lookup sectionName $ intConfig
    Left error -> return . Left . GeneralServerError . show $ error

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetIntegrationConfig sectionName callback = do
  eitherResult <- getIntegrationConfig sectionName
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error
