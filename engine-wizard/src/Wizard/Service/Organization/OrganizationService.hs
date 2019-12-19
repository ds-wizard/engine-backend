module Wizard.Service.Organization.OrganizationService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import Shared.Model.Error.Error
import Wizard.Api.Resource.Organization.OrganizationChangeDTO
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Database.DAO.Organization.OrganizationDAO
import Wizard.LensesConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Organization.OrganizationMapper
import Wizard.Service.Organization.OrganizationValidation

getOrganization :: AppContextM (Either AppError OrganizationDTO)
getOrganization = heFindOrganization $ \organization -> return . Right . toDTO $ organization

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
