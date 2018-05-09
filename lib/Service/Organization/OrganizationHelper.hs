module Service.Organization.OrganizationHelper where

import Service.Organization.OrganizationValidation

hValidateOrganizationDto organizationDto callback =
  case validateOrganizationDto organizationDto of
    Nothing -> callback
    Just error -> return . Left $ error
