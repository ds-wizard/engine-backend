module Wizard.Service.Registry.RegistryUtil where

import qualified Data.List as L

import Wizard.Model.Registry.RegistryOrganization

getDiffOrganizations :: [RegistryOrganization] -> [RegistryOrganization] -> [RegistryOrganization]
getDiffOrganizations orgsRemote orgsLocal = foldl go [] orgsRemote
  where
    go :: [RegistryOrganization] -> RegistryOrganization -> [RegistryOrganization]
    go acc orgRemote =
      case L.find (\orgLocal -> orgRemote.organizationId == orgLocal.organizationId) orgsLocal of
        Just _ -> acc
        Nothing -> orgRemote : acc
