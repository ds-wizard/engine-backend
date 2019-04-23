module Service.Config.IntegrationConfigService
  ( loadConfig
  -- Helpers
  , heLoadConfig
  ) where

import Control.Lens ((^.))
import Control.Monad.Except (join, runExceptT)
import Control.Monad.Reader (asks, liftIO)
import Data.ConfigFile
       (CPError, emptyCP, has_section, items, readfile)
import Data.Map.Strict as M

import LensesConfig hiding (items)
import Model.Config.AppConfig
import Model.Context.AppContext
import Model.Error.Error

loadConfig :: String -> AppContextM (Either AppError (M.Map String String))
loadConfig sectionName = do
  appConfig <- asks _appContextConfig
  loadIntConfig appConfig sectionName >>= either (return . Left . GeneralServerError . show) (return . Right)

-- --------------------------------
-- PRIVATE
-- --------------------------------
loadIntConfig :: AppConfig -> String -> AppContextM (Either CPError (M.Map String String))
loadIntConfig appConfig sectionName =
  liftIO . runExceptT $ do
    let integrationConfigFileName = appConfig ^. integration . config
    configParser <- join . liftIO $ readfile emptyCP ("config/" ++ integrationConfigFileName)
    if has_section configParser sectionName
      then items configParser sectionName >>= return . M.fromList
      else return M.empty

-- --------------------------------
-- HELPERS
-- --------------------------------
heLoadConfig sectionName callback = do
  eitherResult <- loadConfig sectionName
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error
