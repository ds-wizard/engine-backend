module Wizard.Service.Config.AppConfigValidation where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext

validateAppConfig :: AppConfig -> AppContextM ()
validateAppConfig appConfig = validateOrganization (appConfig ^. organization)

validateOrganization :: AppConfigOrganization -> AppContextM ()
validateOrganization config = forM_ (isValidOrganizationId $ config ^. organizationId) throwError

isValidOrganizationId :: String -> Maybe AppError
isValidOrganizationId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT])
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"
