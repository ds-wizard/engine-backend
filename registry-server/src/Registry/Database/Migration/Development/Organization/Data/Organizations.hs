module Registry.Database.Migration.Development.Organization.Data.Organizations where

import Registry.Api.Resource.Organization.OrganizationChangeDTO
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.Organization

orgGlobalEditedChange :: OrganizationChangeDTO
orgGlobalEditedChange =
  OrganizationChangeDTO
    { name = orgGlobalEdited.name
    , description = orgGlobalEdited.description
    , email = orgGlobalEdited.email
    }
