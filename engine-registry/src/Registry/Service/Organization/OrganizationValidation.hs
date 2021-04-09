module Registry.Service.Organization.OrganizationValidation where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Database.DAO.Organization.OrganizationSqlDAO
import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext
import Shared.Model.Error.Error

validateOrganizationCreateDto :: OrganizationCreateDTO -> AppContextM ()
validateOrganizationCreateDto reqDto = do
  _ <- validateOrganizationIdUniqueness (reqDto ^. organizationId)
  _ <- validateOrganizationEmailUniqueness (reqDto ^. email)
  forM_ (validateOrganizationId (reqDto ^. organizationId)) throwError

validateOrganizationId :: String -> Maybe AppError
validateOrganizationId orgId =
  if isJust $ matchRegex validationRegex orgId
    then Nothing
    else Just $ ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__INVALID_ORGANIZATION_ID_FORMAT])
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"

validateOrganizationIdUniqueness :: String -> AppContextM ()
validateOrganizationIdUniqueness orgId = do
  mOrg <- findOrganizationByOrgId' orgId
  case mOrg of
    Just _ ->
      throwError $
      ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__ORGANIZATION_ID_UNIQUENESS orgId])
    Nothing -> return ()

validateOrganizationEmailUniqueness :: String -> AppContextM ()
validateOrganizationEmailUniqueness email = do
  mOrg <- findOrganizationByEmail' email
  case mOrg of
    Just _ ->
      throwError $ ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__ORGANIZATION_EMAIL_UNIQUENESS email])
    Nothing -> return ()

validateOrganizationEmailExistence :: String -> AppContextM ()
validateOrganizationEmailExistence email = do
  mOrg <- findOrganizationByEmail' email
  case mOrg of
    Just _ -> return ()
    Nothing -> throwError $ UserError (_ERROR_VALIDATION__ORGANIZATION_EMAIL_ABSENCE email)

validateOrganizationChangedEmailUniqueness :: String -> String -> AppContextM ()
validateOrganizationChangedEmailUniqueness newEmail oldEmail =
  when (newEmail /= oldEmail) $ validateOrganizationEmailUniqueness newEmail
