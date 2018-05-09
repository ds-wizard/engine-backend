module Service.Organization.OrganizationService where

import Api.Resource.Organization.OrganizationDTO
import Common.Context
import Common.Error
import Database.DAO.Organization.OrganizationDAO
import Service.Organization.OrganizationHelper
import Service.Organization.OrganizationMapper

getOrganization :: Context -> IO (Either AppError OrganizationDTO)
getOrganization context = do
  eitherOrganization <- findOrganization context
  case eitherOrganization of
    Right organization -> return . Right . toDTO $ organization
    Left error -> return . Left $ error

modifyOrganization :: Context -> OrganizationDTO -> IO (Either AppError OrganizationDTO)
modifyOrganization context organizationDto =
  hValidateOrganizationDto organizationDto $ do
    let organization = fromDTO organizationDto
    updateOrganization context organization
    return . Right $ organizationDto
