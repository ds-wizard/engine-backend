module Service.Organization.OrganizationService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationDTO
import Database.DAO.Organization.OrganizationDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Service.Organization.OrganizationMapper
import Service.Organization.OrganizationValidation

getOrganization :: AppContextM (Either AppError OrganizationDTO)
getOrganization = do
  eitherOrganization <- findOrganization
  case eitherOrganization of
    Right organization -> return . Right . toDTO $ organization
    Left error -> return . Left $ error

modifyOrganization :: OrganizationChangeDTO -> AppContextM (Either AppError OrganizationDTO)
modifyOrganization reqDto =
  hmValidateOrganizationDto reqDto $
  heGetOrganization $ \organizationFromDB -> do
    now <- liftIO getCurrentTime
    let organization = fromDTO reqDto (organizationFromDB ^. createdAt) now
    updateOrganization organization
    return . Right . toDTO $ organization

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetOrganization callback = do
  eitherOrganization <- getOrganization
  case eitherOrganization of
    Right organization -> callback organization
    Left error -> return . Left $ error
