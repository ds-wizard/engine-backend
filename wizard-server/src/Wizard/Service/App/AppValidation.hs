module Wizard.Service.App.AppValidation where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import GHC.Unicode (isAlphaNum)
import Text.Regex (matchRegex, mkRegex)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.App.AppChangeDTO
import Wizard.Api.Resource.App.AppCreateDTO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.App.App
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Common

validateAppCreateDTO :: AppCreateDTO -> Bool -> AppContextM ()
validateAppCreateDTO reqDto isAdmin = do
  unless isAdmin validatePublicRegistrationEnabled
  validateAppId reqDto.appId

validatePublicRegistrationEnabled :: AppContextM ()
validatePublicRegistrationEnabled = checkIfServerFeatureIsEnabled "App Registration" (\s -> s.cloud.publicRegistrationEnabled)

validateAppId :: String -> AppContextM ()
validateAppId appId = do
  validateAppIdFormat appId
  validateAppIdUniqueness appId

validateAppChangeDTO :: App -> AppChangeDTO -> AppContextM ()
validateAppChangeDTO app reqDto = do
  validateAppIdFormat reqDto.appId
  when (app.appId /= reqDto.appId) (validateAppIdUniqueness reqDto.appId)

validateAppIdFormat :: String -> AppContextM ()
validateAppIdFormat appId = forM_ (isValidAppIdFormat appId) throwError

isValidAppIdFormat :: String -> Maybe AppError
isValidAppIdFormat appId =
  if not (null appId) && isAlphaNum (head appId) && isAlphaNum (last appId) && isJust (matchRegex validationRegex appId)
    then Nothing
    else Just $ ValidationError [] (M.singleton "appId" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS appId])
  where
    validationRegex = mkRegex "^[a-z0-9-]+$"

validateAppIdUniqueness :: String -> AppContextM ()
validateAppIdUniqueness aId = do
  apps <- findApps
  let usedAppIds = fmap (.appId) apps ++ forbiddenAppIds
  when
    (aId `elem` usedAppIds)
    (throwError . ValidationError [] $ M.singleton "appId" [_ERROR_VALIDATION__APP_ID_UNIQUENESS])

forbiddenAppIds =
  [ "app"
  , "chronograf"
  , "cloud"
  , "czech"
  , "dashboard"
  , "datenzee"
  , "dev"
  , "docker"
  , "e2e"
  , "files"
  , "fujtajbl"
  , "grafana"
  , "ideas"
  , "integrations"
  , "keyclock"
  , "kibana"
  , "ldap"
  , "mockserver"
  , "n8n"
  , "ppe"
  , "provisioning"
  , "rabbitmq"
  , "registry"
  , "registry"
  , "registry-ppe"
  , "registry-staging"
  , "registry-test"
  , "retro"
  , "s3"
  , "staging"
  , "status"
  , "storage-costs-evaluator"
  , "submit"
  , "swarmpit"
  , "www"
  ]
