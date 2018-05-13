module Service.Organization.OrganizationService where

import Control.Lens ((^.))
import Data.Time

import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationDTO
import Common.Context
import Common.Error
import Database.DAO.Organization.OrganizationDAO
import LensesConfig
import Service.Organization.OrganizationMapper
import Service.Organization.OrganizationValidation

getOrganization :: Context -> IO (Either AppError OrganizationDTO)
getOrganization context = do
  eitherOrganization <- findOrganization context
  case eitherOrganization of
    Right organization -> return . Right . toDTO $ organization
    Left error -> return . Left $ error

modifyOrganization :: Context -> OrganizationChangeDTO -> IO (Either AppError OrganizationDTO)
modifyOrganization context reqDto =
  hmValidateOrganizationDto reqDto $
  heGetOrganization context $ \organizationFromDB -> do
    now <- getCurrentTime
    let organization = fromDTO reqDto (organizationFromDB ^. createdAt) now
    updateOrganization context organization
    return . Right . toDTO $ organization

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetOrganization context callback = do
  eitherOrganization <- getOrganization context
  case eitherOrganization of
    Right organization -> callback organization
    Left error -> return . Left $ error
